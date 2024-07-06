REPORT zoa2c_zazure_by_destination.

DATA: destination TYPE pficf_destination_name,
      http_method TYPE string,
      param_kind  TYPE string,
      params      TYPE tihttpnvp,
      param       TYPE ihttpnvp.

PARAMETERS: p_dest   TYPE pficf_destination_name DEFAULT 'MS_GRAPH',
            p_target TYPE string LOWER CASE DEFAULT `/users/{id | userPrincipalName}/profile`.

AT SELECTION-SCREEN.

START-OF-SELECTION.
  destination = p_dest.
  http_method  = `GET`.
  param_kind = 'H'.

  DATA: http_client   TYPE REF TO if_http_client,
        status_code   TYPE i,
        response_data TYPE string,
        fields        TYPE tihttpnvp,
        content_type  TYPE string.

  FIELD-SYMBOLS <field> LIKE LINE OF fields.

**********************************************************************
* CREATE http client
**********************************************************************
  cl_http_client=>create_by_destination(
      EXPORTING
        destination                = destination
      IMPORTING
        client                     = http_client
      EXCEPTIONS
        argument_not_found         = 1
        destination_not_found      = 2
        destination_no_authority   = 3
        plugin_not_active          = 4
        internal_error             = 5
        oa2c_set_token_error       = 6
        oa2c_missing_authorization = 7
        oa2c_invalid_config        = 8
        oa2c_invalid_parameters    = 9
        oa2c_invalid_scope         = 10
        oa2c_invalid_grant         = 11
        OTHERS                     = 12 ).

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.


  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

* turn off logon popup. detect authentication errors.
  http_client->propertytype_logon_popup = 0.

  http_client->request->set_method( http_method ).

  LOOP AT params INTO param.
    http_client->request->set_form_field(
          name  = param-name
          value = param-value ).
  ENDLOOP.

**********************************************************************
* Send / Receive Request
**********************************************************************
  http_client->request->set_header_field(
    name  = '~request_uri'
    value = p_target ).

  http_client->send(
    EXCEPTIONS
      http_communication_failure = 1
      http_invalid_state         = 2
      http_processing_failed     = 3
      http_invalid_timeout       = 4
      OTHERS                     = 5 ).
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
  http_client->receive(
    EXCEPTIONS
      http_communication_failure = 1
      http_invalid_state         = 2
      http_processing_failed     = 3
      OTHERS                     = 4 ).
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

**********************************************************************
* Display result
**********************************************************************
  http_client->response->get_status( IMPORTING code = status_code ).
  WRITE / |{ status_code }|.

  WRITE /.

  IF status_code = 200.
    response_data = http_client->response->get_cdata( ).
    content_type = http_client->response->get_content_type( ).
    IF content_type CP `text/html*`.
      cl_demo_output=>display_html( html = response_data ).
    ELSEIF content_type CP `text/xml*`.
      cl_demo_output=>display_xml( xml = response_data ).
    ELSEIF content_type CP `application/json*`.
      cl_demo_output=>display_json( json = response_data ).
    ENDIF.
  ELSE.
    http_client->response->get_header_fields( CHANGING fields = fields ).
    LOOP AT fields ASSIGNING <field>.
      WRITE: / <field>-name, 25 <field>-value.
    ENDLOOP.

  ENDIF.


**********************************************************************
* Close
**********************************************************************
  http_client->close(
    EXCEPTIONS
      http_invalid_state = 1
      OTHERS             = 2 ).
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
