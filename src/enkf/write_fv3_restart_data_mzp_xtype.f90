  CALL nc_check( nf90_inq_varid(file_id,TRIM(ADJUSTL(varname)),var_id),&
       myname_,'inq_varid '//TRIM(ADJUSTL(varname))//' '//TRIM(filename) )
  CALL nc_check( nf90_inquire_variable(file_id,var_id,xtype=xtype),&
       myname_,'inquire_variable '//TRIM(ADJUSTL(varname))//' '//TRIM(filename) )

!may be unnecessary
  IF (xtype == nf_real) THEN
     CALL nc_check( nf90_put_var(file_id,var_id,REAL(tmp)),&
          myname_,'put_var '//TRIM(ADJUSTL(varname))//' '//TRIM(filename) )
  ELSEIF (xtype == nf_double) THEN  
     CALL nc_check( nf90_put_var(file_id,var_id,tmp),&
          myname_,'put_var '//TRIM(ADJUSTL(varname))//' '//TRIM(filename) )
  ELSE
     WRITE(6,*)'put_var undefined type - stopping'
     call stop2(719)
  ENDIF
