CLASS zcl_oa2c_ce_zazure DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_badi_interface .
    INTERFACES if_oa2c_config_extension .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_oa2c_ce_zazure IMPLEMENTATION.


  METHOD if_oa2c_config_extension~get_ac_auth_requ_params.
    DATA: additional_param LIKE LINE OF et_additional_params.

    additional_param-name  = `resource`.
    additional_param-value = `https://graph.windows.net`.
    APPEND additional_param TO et_additional_params.

    additional_param-name  = `prompt`.
    additional_param-value = `consent`.
    APPEND additional_param TO et_additional_params.
  ENDMETHOD.
ENDCLASS.
