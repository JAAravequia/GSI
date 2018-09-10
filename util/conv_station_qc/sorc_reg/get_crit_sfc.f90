!  this program try to get criteria for bias and rejection


   subroutine get_crit_sfc(dtype,filetype,ct)

   use crite_constnt_reg,only: ps_c_rm,ps_c_std,q_c_rm,q_c_std,t3_c_rm,t3_c_std,t4_c_rm,t4_c_std,wd_c_rm,wd_c_std,ws_c_rm,ws_c_std
 
   character(5) dtype,filetype 
   character(1) ddtype

   ddtype=dtype(1:1)
   ddtype=trim(ddtype)
  

!  print *,'ddtype,filetype=',ddtype,filetype
   if( ddtype == 'p' .and. trim(filetype)=='bias') then
      ct= ps_c_rm
   else if( ddtype == 'p' .and. trim(filetype)=='rej') then
      ct= ps_c_std
   else if(ddtype == 'q' .and. trim(filetype)=='bias') then
      ct= q_c_rm
   else if( ddtype == 'q' .and. trim(filetype)=='rej') then
      ct= q_c_std
   else if(ddtype == 't' .and. trim(dtype) == 't180' .and.  trim(filetype)== 'bias') then
      ct= t3_c_rm
   else if (ddtype == 't' .and. trim(filetype)=='bias') then
       ct= t4_c_rm
   else if( ddtype == 't' .and. trim(dtype) == 't180' .and. trim(filetype)=='rej') then
      ct= t3_c_std
   else if( ddtype == 't' .and.  trim(filetype)=='rej') then
      ct= t4_c_std
   else if( ddtype == 'u' .and. trim(filetype)=='rej') then
      ct= wd_c_std
   endif


!    print *,'ct=',ct

    return
    end

   
   

   

