REPORT zmsazure LINE-SIZE 1023.


DATA: profile     TYPE oa2c_profile,
      tennant     TYPE string,
      target      TYPE string,
      http_method TYPE string,
      param_kind  TYPE string,
      params      TYPE tihttpnvp,
      param       TYPE ihttpnvp.

AT SELECTION-SCREEN.

START-OF-SELECTION.

  profile = 'ZAZURE1'.
  tennant = ''.
  target  = `https://graph.windows.net/` && tennant && `/users?api-version=1.6`.
  http_method  = `GET`.
  param_kind = 'H'.

  DATA: http_client    TYPE REF TO if_http_client,
        oa2c_client    TYPE REF TO if_oauth2_client,
        status_code    TYPE i,
        response_data  TYPE string,
        fields         TYPE tihttpnvp,
        oa2c_exception TYPE REF TO cx_oa2c.

  FIELD-SYMBOLS: <ls_field> LIKE LINE OF fields.


**********************************************************************
* CREATE http client
**********************************************************************
  CALL METHOD cl_http_client=>create_by_url
    EXPORTING
      url                = target
      ssl_id             = 'ANONYM'
    IMPORTING
      client             = http_client
    EXCEPTIONS
      argument_not_found = 1
      plugin_not_active  = 2
      internal_error     = 3
      OTHERS             = 4.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

* turn off logon popup. detect authentication errors.
  http_client->propertytype_logon_popup = 0.

  CALL METHOD http_client->request->set_method
    EXPORTING
      method = http_method.

  LOOP AT params INTO param.
    CALL METHOD http_client->request->set_form_field
      EXPORTING
        name  = param-name
        value = param-value.
  ENDLOOP.


**********************************************************************
* Set oauth 2.0 token
**********************************************************************
  TRY.

      CALL METHOD cl_oauth2_client=>create
        EXPORTING
          i_profile        = profile
        RECEIVING
          ro_oauth2_client = oa2c_client.

    CATCH cx_oa2c INTO oa2c_exception.
      WRITE: `Error calling CREATE.`.
      WRITE: / oa2c_exception->get_text( ).
      RETURN.
  ENDTRY.

  TRY.

      CALL METHOD oa2c_client->set_token
        EXPORTING
          io_http_client = http_client
          i_param_kind   = param_kind.

    CATCH cx_oa2c INTO oa2c_exception.
      TRY.
          CALL METHOD oa2c_client->execute_refresh_flow.
        CATCH cx_oa2c INTO oa2c_exception.
          WRITE: `Error calling EXECUTE_REFRESH_FLOW.`.
          WRITE: / oa2c_exception->get_text( ).
          RETURN.
      ENDTRY.
      TRY.
          CALL METHOD oa2c_client->set_token
            EXPORTING
              io_http_client = http_client
              i_param_kind   = param_kind.
        CATCH cx_oa2c INTO oa2c_exception.
          WRITE: `Error calling SET_TOKEN.`.
          WRITE: / oa2c_exception->get_text( ).
          RETURN.
      ENDTRY.
  ENDTRY.


**********************************************************************
* Send / Receive Request
**********************************************************************
  CALL METHOD http_client->send
    EXCEPTIONS
      http_communication_failure = 1
      http_invalid_state         = 2
      http_processing_failed     = 3
      http_invalid_timeout       = 4
      OTHERS                     = 5.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  CALL METHOD http_client->receive
    EXCEPTIONS
      http_communication_failure = 1
      http_invalid_state         = 2
      http_processing_failed     = 3
      OTHERS                     = 4.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.


**********************************************************************
* Display result
**********************************************************************
  CALL METHOD http_client->response->get_status
    IMPORTING
      code = status_code.
  WRITE / |{ status_code }|.

  WRITE /.

  IF status_code = 200.
    CALL METHOD http_client->response->get_cdata
      RECEIVING
        data = response_data.

    DATA(l_content_type) = http_client->response->get_content_type( ).
    IF l_content_type CP `text/html*`.
      cl_demo_output=>display_html( html = response_data ).
    ELSEIF l_content_type CP `text/xml*`.
      cl_demo_output=>display_xml( xml = response_data ).
    ELSEIF l_content_type CP `application/json*`.
      cl_demo_output=>display_json( json = response_data ).
    ENDIF.
  ELSE.
    CALL METHOD http_client->response->get_header_fields
      CHANGING
        fields = fields.

    LOOP AT fields ASSIGNING <ls_field>.
      WRITE: / <ls_field>-name, 25 <ls_field>-value.
    ENDLOOP.

  ENDIF.


**********************************************************************
* Close
**********************************************************************
  CALL METHOD http_client->close
    EXCEPTIONS
      http_invalid_state = 1
      OTHERS             = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
