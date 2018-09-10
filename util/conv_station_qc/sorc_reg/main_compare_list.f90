! the program is sorting out list for different time delay analysis

   character(5) dtype,filetype
   character(1) ddtype
   logical,dimension(3) :: fileexist
  character(2),dimension(3) :: tm 

   namelist/input/ntm,fileexist,tm,dtype,filetype,itype

   read(5,input)
     write(6,input)
     write(6,*)' '


   ddtype=dtype(1:1)

    mfile=0              ! total how many bias or rejection list files, mfile <=ntm
   do i=1,ntm
   if(fileexist(i)) mfile=mfile+1
  enddo



  if (mfile >0) then
     if(itype == 0 .and. ddtype == 'u') then
        call compare_list_sfc(ntm,dtype,tm,fileexist,filetype,4,4)
     else if (itype == 0 .and. ddtype /= 'u') then
        call compare_list_sfc(ntm,dtype,tm,fileexist,filetype,2,4)
     else if(itype == 1 .and. ddtype == 'q') then
        nk=29
        call compare_list(ntm,dtype,tm,fileexist,filetype,2,4,nk)
     else if(itype == 1 .and. ddtype == 'u') then
        nk=43
        call compare_list(ntm,dtype,tm,fileexist,filetype,4,4,nk)
     else
        nk=43
        call compare_list(ntm,dtype,tm,fileexist,filetype,2,4,nk)
     endif
  else
    print *,'main_compare_list: there is no ',filetype,' file in all analysis time'
  endif



  stop
  end
