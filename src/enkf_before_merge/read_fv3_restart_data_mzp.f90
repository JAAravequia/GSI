  CALL nc_check( nf90_inq_varid(file_id,TRIM(ADJUSTL(varname)),var_id),&
       myname_,'inq_varid '//TRIM(ADJUSTL(varname))//' '//TRIM(filename) )
  CALL nc_check( nf90_get_var(file_id,var_id,tmp),&
       myname_,'get_var '//TRIM(ADJUSTL(varname))//' '//TRIM(filename) )
  
