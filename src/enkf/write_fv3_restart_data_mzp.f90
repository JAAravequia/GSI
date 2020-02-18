  CALL nc_check( nf90_inq_varid(file_id,TRIM(ADJUSTL(varname)),var_id),&
       myname_,'inq_varid '//TRIM(ADJUSTL(varname))//' '//TRIM(filename) )
  CALL nc_check( nf90_put_var(file_id,var_id,REAL(tmp)),&
       myname_,'put_var '//TRIM(ADJUSTL(varname))//' '//TRIM(filename) )
