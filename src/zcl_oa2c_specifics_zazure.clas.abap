CLASS zcl_oa2c_specifics_zazure DEFINITION
  PUBLIC
  INHERITING FROM cl_oa2c_specifics_abstract
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS if_oa2c_specifics~get_config_extension
        REDEFINITION .
    METHODS if_oa2c_specifics~get_endpoint_settings
        REDEFINITION .
    METHODS if_oa2c_specifics~get_supported_grant_types
        REDEFINITION .
    METHODS if_oa2c_specifics~get_ac_auth_requ_param_names
        REDEFINITION .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_oa2c_specifics_zazure IMPLEMENTATION.


  METHOD if_oa2c_specifics~get_ac_auth_requ_param_names.

    DATA: ls_add_param TYPE if_oa2c_specifics~ty_s_add_param.

    super->if_oa2c_specifics~get_ac_auth_requ_param_names(
      IMPORTING
        e_client_id           = e_client_id
        e_redirect_uri        = e_redirect_uri
        e_response_type       = e_response_type
        e_response_type_value = e_response_type_value
        e_scope               = e_scope
    ).

    ls_add_param-name = `resource`.
    INSERT ls_add_param INTO TABLE et_add_param_names.
    ls_add_param-name = `prompt`.
    INSERT ls_add_param INTO TABLE et_add_param_names.

  ENDMETHOD.


  METHOD if_oa2c_specifics~get_config_extension.
    r_config_extension = `ZAZURE`.
  ENDMETHOD.


  METHOD if_oa2c_specifics~get_endpoint_settings.
    CONSTANTS app_id TYPE string VALUE ''.

    e_changeable                  = abap_true.

    e_authorization_endpoint_path = `login.microsoftonline.com/` && app_id && `/oauth2/v2.0/authorize`.
    e_token_endpoint_path         = `login.microsoftonline.com/` && app_id && `/oauth2/v2.0/token`.
    CLEAR e_revocation_endpoint_path.

  ENDMETHOD.


  METHOD if_oa2c_specifics~get_supported_grant_types.
    e_authorization_code = abap_true.
    e_saml20_assertion   = abap_false.
    e_refresh            = abap_true.
    e_revocation         = abap_false.
    e_cc                 = abap_true.
  ENDMETHOD.
ENDCLASS.
