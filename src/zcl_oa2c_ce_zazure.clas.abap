class ZCL_OA2C_CE_ZAZURE definition
  public
  create public .

public section.

  interfaces IF_BADI_INTERFACE .
  interfaces IF_OA2C_CONFIG_EXTENSION .
protected section.
private section.
ENDCLASS.



CLASS ZCL_OA2C_CE_ZAZURE IMPLEMENTATION.


  METHOD if_oa2c_config_extension~get_ac_auth_requ_params.
    DATA: ls_nvp LIKE LINE OF et_additional_params.

    ls_nvp-name  = `resource`.
    ls_nvp-value = `https://graph.windows.net`.
    APPEND ls_nvp TO et_additional_params.

    ls_nvp-name  = `prompt`.
    ls_nvp-value = `consent`.
    APPEND ls_nvp TO et_additional_params.
  ENDMETHOD.
ENDCLASS.
