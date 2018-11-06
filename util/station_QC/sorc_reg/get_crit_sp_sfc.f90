!  this program try to get criteria for bias and rejection


   subroutine get_crit_sp_sfc(dtype,filetype,ct)

   use crite_constnt_reg,only: wd_c_rm,wd_c_std,ws_c_rm,ws_c_std
 
   character(5) dtype,filetype 
   character(1) ddtype

   ddtype=dtype(1:1)
   ddtype=trim(ddtype)
  

!  print *,'get_crit_sp_sfc:ddtype,filetype=',ddtype,filetype
      if (trim(filetype) == 'sp') then
         ct= ws_c_std
      else if(trim(filetype) == 'dir') then
         ct= wd_c_std
      endif



!  print *,'get_crit_sp_sfc:ddtype,filetype=',ddtype,filetype,ct
    return
    end

   
   

   

