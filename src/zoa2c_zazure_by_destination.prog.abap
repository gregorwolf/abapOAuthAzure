*&---------------------------------------------------------------------*
*& Report ZOA2C_ZAZURE
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zoa2c_zazure_by_destination.

DATA: destination TYPE pficf_destination_name,
      method      TYPE string,
      param_kind  TYPE string,
      lt_param    TYPE tihttpnvp,
      ls_param    TYPE ihttpnvp.

PARAMETERS: dest   TYPE pficf_destination_name DEFAULT 'MS_GRAPH',
            target TYPE string LOWER CASE DEFAULT `/users/{id | userPrincipalName}/profile`.

AT SELECTION-SCREEN.

START-OF-SELECTION.
  destination = dest.
  method  = `GET`.
  param_kind = 'H'.

  DATA: lo_http_client  TYPE REF TO if_http_client,
        lo_oa2c_client  TYPE REF TO if_oauth2_client,
        l_status_code   TYPE i,
        l_response_data TYPE string,
        lt_fields       TYPE tihttpnvp,
        lx_oa2c         TYPE REF TO cx_oa2c.

  FIELD-SYMBOLS: <ls_field> LIKE LINE OF lt_fields.

**********************************************************************
* CREATE http client
**********************************************************************
  cl_http_client=>create_by_destination(
      EXPORTING
        destination                = destination
      IMPORTING
        client                     = lo_http_client
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
        OTHERS                     = 12
  ).


  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.


  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

* turn off logon popup. detect authentication errors.
  lo_http_client->propertytype_logon_popup = 0.

  lo_http_client->request->set_method(
    method = method
  ).

  LOOP AT lt_param INTO ls_param.
    lo_http_client->request->set_form_field(
      EXPORTING
          name  = ls_param-name
          value = ls_param-value
    ).
  ENDLOOP.

**********************************************************************
* Send / Receive Request
**********************************************************************
  lo_http_client->request->set_header_field(
    name  = '~request_uri'
    value = target ).

  lo_http_client->send(
    EXCEPTIONS
      http_communication_failure = 1
      http_invalid_state         = 2
      http_processing_failed     = 3
      http_invalid_timeout       = 4
      OTHERS                     = 5
  ).
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
  lo_http_client->receive(
    EXCEPTIONS
      http_communication_failure = 1
      http_invalid_state         = 2
      http_processing_failed     = 3
      OTHERS                     = 4
  ).
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.


**********************************************************************
* Display result
**********************************************************************
  lo_http_client->response->get_status(
    IMPORTING
        code = l_status_code
  ).
  WRITE / |{ l_status_code }|.

  WRITE /.

  IF l_status_code = 200.
    l_response_data = lo_http_client->response->get_cdata( ).
    DATA(l_content_type) = lo_http_client->response->get_content_type( ).
    IF l_content_type CP `text/html*`.
      cl_demo_output=>display_html( html = l_response_data ).
    ELSEIF l_content_type CP `text/xml*`.
      cl_demo_output=>display_xml( xml = l_response_data ).
    ELSEIF l_content_type CP `application/json*`.
      cl_demo_output=>display_json( json = l_response_data ).
    ENDIF.
  ELSE.
    lo_http_client->response->get_header_fields(
      CHANGING
        fields = lt_fields
    ).
    LOOP AT lt_fields ASSIGNING <ls_field>.
      WRITE: / <ls_field>-name, 25 <ls_field>-value.
    ENDLOOP.

  ENDIF.


**********************************************************************
* Close
**********************************************************************
  lo_http_client->close(
    EXCEPTIONS
      http_invalid_state = 1
      OTHERS             = 2
  ).
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
