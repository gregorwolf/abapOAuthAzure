class ZCL_OA2C_SPECIFICS_ZAZURE definition
  public
  inheriting from CL_OA2C_SPECIFICS_ABSTRACT
  create public .

public section.

  methods IF_OA2C_SPECIFICS~GET_CONFIG_EXTENSION
    redefinition .
  methods IF_OA2C_SPECIFICS~GET_ENDPOINT_SETTINGS
    redefinition .
  methods IF_OA2C_SPECIFICS~GET_SUPPORTED_GRANT_TYPES
    redefinition .
  methods IF_OA2C_SPECIFICS~GET_AC_AUTH_REQU_PARAM_NAMES
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS ZCL_OA2C_SPECIFICS_ZAZURE IMPLEMENTATION.


  METHOD if_oa2c_specifics~get_ac_auth_requ_param_names.

    DATA: ls_add_param TYPE if_oa2c_specifics~ty_s_add_param.

    CALL METHOD super->if_oa2c_specifics~get_ac_auth_requ_param_names
      IMPORTING
        e_client_id           = e_client_id
        e_redirect_uri        = e_redirect_uri
        e_response_type       = e_response_type
        e_response_type_value = e_response_type_value
        e_scope               = e_scope.

    ls_add_param-name = `resource`.
    INSERT ls_add_param INTO TABLE et_add_param_names.
    ls_add_param-name = `prompt`.
    INSERT ls_add_param INTO TABLE et_add_param_names.

  ENDMETHOD.


  METHOD if_oa2c_specifics~get_config_extension.
    r_config_extension = `ZAZURE`.
  ENDMETHOD.


  METHOD if_oa2c_specifics~get_endpoint_settings.
    CONSTANTS c_app_id TYPE string VALUE ''.

    e_changeable                  = abap_false.

    e_authorization_endpoint_path = `login.windows.net/` && c_app_id && `/oauth2/authorize`.
    e_token_endpoint_path         = `login.windows.net/` && c_app_id && `/oauth2/token`.
    CLEAR e_revocation_endpoint_path.

  ENDMETHOD.


  method IF_OA2C_SPECIFICS~GET_SUPPORTED_GRANT_TYPES.
     e_authorization_code = abap_true.
     e_saml20_assertion   = abap_false.
     e_refresh            = abap_true.
     e_revocation         = abap_false.
  endmethod.
ENDCLASS.
