REPORT zoa2c_zazure_by_url LINE-SIZE 1023.


DATA: http_method TYPE string,
      param_kind  TYPE string,
      params      TYPE tihttpnvp,
      param       TYPE ihttpnvp.

PARAMETERS: profile TYPE oa2c_profile DEFAULT 'ZAZURE1',
            config  TYPE oa2c_configuration,
            target  TYPE string LOWER CASE DEFAULT 'https://graph.windows.net/<Your Microsoft Azure Domain>/me?api-version=2013-04-05'.

AT SELECTION-SCREEN.

START-OF-SELECTION.

  http_method  = `GET`.
  param_kind = 'H'.

  DATA: http_client    TYPE REF TO if_http_client,
        oa2c_client    TYPE REF TO if_oauth2_client,
        status_code    TYPE i,
        response_data  TYPE string,
        fields         TYPE tihttpnvp,
        oa2c_exception TYPE REF TO cx_oa2c.

  FIELD-SYMBOLS <field> LIKE LINE OF fields.


**********************************************************************
* CREATE http client
**********************************************************************
  cl_http_client=>create_by_url(
    EXPORTING
      url                = target
      ssl_id             = 'ANONYM'
    IMPORTING
      client             = http_client
    EXCEPTIONS
      argument_not_found = 1
      plugin_not_active  = 2
      internal_error     = 3
      OTHERS             = 4 ).
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
* Set oauth 2.0 token
**********************************************************************
  TRY.

      oa2c_client = cl_oauth2_client=>create(
        i_profile       = profile
        i_configuration = config ).

    CATCH cx_oa2c INTO oa2c_exception.
      WRITE `Error calling CREATE.`.
      WRITE / oa2c_exception->get_text( ).
      RETURN.
  ENDTRY.

  TRY.
      oa2c_client->set_token(
          io_http_client = http_client
          i_param_kind   = param_kind ).

    CATCH cx_oa2c INTO oa2c_exception.
      WRITE / oa2c_exception->get_text( ).
      TRY.
          oa2c_client->execute_refresh_flow( ).
        CATCH cx_oa2c INTO oa2c_exception.
          WRITE / `Error calling EXECUTE_REFRESH_FLOW.`.
          WRITE / oa2c_exception->get_text( ).
          RETURN.
      ENDTRY.
      TRY.
          oa2c_client->set_token(
              io_http_client = http_client
              i_param_kind   = param_kind ).
        CATCH cx_oa2c INTO oa2c_exception.
          WRITE `Error calling SET_TOKEN.`.
          WRITE / oa2c_exception->get_text( ).
          RETURN.
      ENDTRY.
  ENDTRY.


**********************************************************************
* Send / Receive Request
**********************************************************************
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
  http_client->response->get_status( code = status_code ).
  WRITE / |{ status_code }|.

  WRITE /.

  IF status_code = 200.
    response_data = http_client->response->get_cdata( ).

    DATA(content_type) = http_client->response->get_content_type( ).
    IF content_type CP `text/html*`.
      cl_demo_output=>display_html( html = response_data ).
    ELSEIF content_type CP `text/xml*`.
      cl_demo_output=>display_xml( xml = response_data ).
    ELSEIF content_type CP `application/json*`.
      cl_demo_output=>display_json( json = response_data ).
    ENDIF.
  ELSE.
    http_client->response->get_header_fields( fields = fields ).

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
