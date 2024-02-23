FUNCTION zfm_mara_update.
*"----------------------------------------------------------------------
*"*"Update Function Module:
*"
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IT_UPD) TYPE  MARA OPTIONAL
*"----------------------------------------------------------------------

  IF it_upd IS NOT INITIAL.
    UPDATE mara FROM it_upd.
  ENDIF.

ENDFUNCTION.
