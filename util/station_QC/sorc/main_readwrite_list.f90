!  the program is to read the list for the type not assimilated in global 
!  write the format same as the type assimilated in the global assimilation


    character(10) dtype,filetype,ddtype

   namelist/input/dtype,filetype,itype

   read(5,input)
     write(6,input)
     write(6,*)' '


   ddtype=dtype(1:1)

   if(itype == 0  .and. trim(ddtype) == 'u') then
      call readwrite_list_sfc(dtype,filetype,4,1)
      call readwrite_list_sfc(dtype,filetype,4,2)
   else if(itype == 0  .and. trim(ddtype) /= 'u') then
      call readwrite_list_sfc(dtype,filetype,2,1)
   else if( itype ==1 .and. trim(ddtype) == 'u') then
      call readwrite_list(dtype,filetype,4,1)
      call readwrite_list(dtype,filetype,4,2)
   else if( itype ==1 .and. trim(ddtype) /= 'u') then
      call readwrite_list(dtype,filetype,2,1)
   endif


  stop
  end 
