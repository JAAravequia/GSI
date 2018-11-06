! the program is sorting out list for different time delay analysis

   character(5) dtype,filetype
   character(1) ddtype
   logical,dimension(3) :: fileexist
   character(5),dimension(3) :: tm

   namelist/input/ntm,fileexist,tm,dtype,itype

   read(5,input)
     write(6,input)
     write(6,*)' '

     mfile=0              ! total how many bias or rejection list files, mfile <=ntm

  do i=1,ntm
   if(fileexist(i)) mfile=mfile+1
  enddo



   ddtype=dtype(1:1)

  if (mfile >0) then

   if(itype == 0 .and. ddtype == 'u') then
    call compare_list_sfc_qlyrej(ntm,dtype,tm,fileexist,4,4)
   else if (itype == 0 .and. ddtype /= 'u') then
    call compare_list_sfc_qlyrej(ntm,dtype,tm,fileexist,2,4)
  else if(itype == 1 .and. ddtype == 'q') then
    nk=29
    call compare_list_qlyrej(ntm,dtype,tm,fileexist,2,4,nk)
  else if(itype == 1 .and. ddtype == 'u') then
    nk=43
    call compare_list_qlyrej(ntm,dtype,tm,fileexist,4,4,nk)
  else
    nk=43
    call compare_list_qlyrej(ntm,dtype,tm,fileexist,2,4,nk)
  endif

else
    print *,'main_compare_list_qlyrej: there is no ',filetype,' file in all analysis time'
  endif



  stop
  end
