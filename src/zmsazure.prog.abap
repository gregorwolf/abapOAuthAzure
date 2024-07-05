REPORT zmsazure LINE-SIZE 1023.


DATA: profile    TYPE oa2c_profile,
      tennant    TYPE string,
      target     TYPE string,
      method     TYPE string,
      param_kind TYPE string,
      lt_param   TYPE tihttpnvp,
      ls_param   TYPE ihttpnvp.

AT SELECTION-SCREEN.

START-OF-SELECTION.

  profile = 'ZAZURE1'.
  tennant = ''.
  target  = `https://graph.windows.net/` && tennant && `/users?api-version=1.6`.
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
  CALL METHOD cl_http_client=>create_by_url
    EXPORTING
      url                = target
      ssl_id             = 'ANONYM'
    IMPORTING
      client             = lo_http_client
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
  lo_http_client->propertytype_logon_popup = 0.

  CALL METHOD lo_http_client->request->set_method
    EXPORTING
      method = method.

  LOOP AT lt_param INTO ls_param.
    CALL METHOD lo_http_client->request->set_form_field
      EXPORTING
        name  = ls_param-name
        value = ls_param-value.
  ENDLOOP.


**********************************************************************
* Set oauth 2.0 token
**********************************************************************
  TRY.

      CALL METHOD cl_oauth2_client=>create
        EXPORTING
          i_profile        = profile
        RECEIVING
          ro_oauth2_client = lo_oa2c_client.

    CATCH cx_oa2c INTO lx_oa2c.
      WRITE: `Error calling CREATE.`.
      WRITE: / lx_oa2c->get_text( ).
      RETURN.
  ENDTRY.

  TRY.

      CALL METHOD lo_oa2c_client->set_token
        EXPORTING
          io_http_client = lo_http_client
          i_param_kind   = param_kind.

    CATCH cx_oa2c INTO lx_oa2c.
      TRY.
          CALL METHOD lo_oa2c_client->execute_refresh_flow.
        CATCH cx_oa2c INTO lx_oa2c.
          WRITE: `Error calling EXECUTE_REFRESH_FLOW.`.
          WRITE: / lx_oa2c->get_text( ).
          RETURN.
      ENDTRY.
      TRY.
          CALL METHOD lo_oa2c_client->set_token
            EXPORTING
              io_http_client = lo_http_client
              i_param_kind   = param_kind.
        CATCH cx_oa2c INTO lx_oa2c.
          WRITE: `Error calling SET_TOKEN.`.
          WRITE: / lx_oa2c->get_text( ).
          RETURN.
      ENDTRY.
  ENDTRY.


**********************************************************************
* Send / Receive Request
**********************************************************************
  CALL METHOD lo_http_client->send
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

  CALL METHOD lo_http_client->receive
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
  CALL METHOD lo_http_client->response->get_status
    IMPORTING
      code = l_status_code.
  WRITE / |{ l_status_code }|.

  WRITE /.

  IF l_status_code = 200.
    CALL METHOD lo_http_client->response->get_cdata
      RECEIVING
        data = l_response_data.

    DATA(l_content_type) = lo_http_client->response->get_content_type( ).
    IF l_content_type CP `text/html*`.
      cl_demo_output=>display_html( html = l_response_data ).
    ELSEIF l_content_type CP `text/xml*`.
      cl_demo_output=>display_xml( xml = l_response_data ).
    ELSEIF l_content_type CP `application/json*`.
      cl_demo_output=>display_json( json = l_response_data ).
    ENDIF.
  ELSE.
    CALL METHOD lo_http_client->response->get_header_fields
      CHANGING
        fields = lt_fields.

    LOOP AT lt_fields ASSIGNING <ls_field>.
      WRITE: / <ls_field>-name, 25 <ls_field>-value.
    ENDLOOP.

  ENDIF.


**********************************************************************
* Close
**********************************************************************
  CALL METHOD lo_http_client->close
    EXCEPTIONS
      http_invalid_state = 1
      OTHERS             = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
