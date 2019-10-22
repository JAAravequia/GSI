  CHARACTER(len=*), INTENT(in) :: varname
  CHARACTER(len=*), INTENT(in) :: filename
  INTEGER(i_kind), INTENT(in) :: file_id
  INTEGER(i_kind) :: var_id

  CALL nc_check( nf90_inq_varid(file_id,TRIM(ADJUSTL(varname)),var_id),&
       myname_,'inq_varid '//TRIM(ADJUSTL(varname))//' '//TRIM(filename) )
  CALL nc_check( nf90_get_var(file_id,var_id,data_arr),&
       myname_,'get_var '//TRIM(ADJUSTL(varname))//' '//TRIM(filename) )
  
