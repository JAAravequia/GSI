!    read the bias and rejection list

     
     subroutine readwrite_list_sfc(dtype,filetype,nd,itype)
    
    
     character(10) dtype,ddtype,filetype
     character(8) stationid
     character(50) file,file2
     real(4) rlon,rlat,stdno,rmeanb,rmeana,stdb,stda,tdatano
     real(4) rmdb,rmda,rmsb,rmsa,stddb,stdda,stdsb,stdsa
     real(4) tdatano2,xmedb,xmeda,smadb,smada,breakb1,breakb2,breake1,breake2
     integer iqcflg
     


    Print *,' read_list2_sfc: nob=',nob
     ncount=0

     ddtype=dtype(1:1)
     ddtype=trim(ddtype)
    
    if( trim(filetype) == 'qlyrej' ) then  
       file=trim(dtype)//'_'//trim(filetype)//'_list'
       file2=trim(dtype)//'_'//trim(filetype)//'_list_final'
    else if(trim(ddtype) /= 'u') then
       file=trim(dtype)//'_'//trim(filetype)//'_list'
       file2=trim(dtype)//'_'//trim(filetype)//'_list_final'  
    else if(trim(ddtype) == 'u' .and. itype ==1) then
       file=trim(dtype)//'_'//trim(filetype)//'_dir_list'
       file2=trim(dtype)//'_'//trim(filetype)//'_dir_list_final'
     else if (trim(ddtype) == 'u' .and. itype ==2) then
       file=trim(dtype)//'_'//trim(filetype)//'_sp_list'
       file2=trim(dtype)//'_'//trim(filetype)//'_sp_list_final'
     endif

     open (11,file=file,form='formatted')
     open (21,file=file2,form='formatted')
     

  loopd: do

    if( nd ==2) then
       read(11,100,IOSTAT=iflag) stationid,rlon,rlat,rmeanb,rmeana,stdb,stda,iqcflg
       if(iflag /=0 ) exit
!       print *, stationid,rlon,rlat,stdno,rmeanb,rmeana,stdb,stda,tdatano,tdatano2
        write(21,100) stationid,rlon,rlat,rmeanb,rmeana,stdb,stda,iqcflg
       
     else if(nd ==4) then
       read(11,150,IOSTAT=iflag) stationid,rlon,rlat,rmdb,rmda,rmsb,rmsa,stddb,stdda,stdsb,stdsa,iqcflg
       if(iflag /=0 ) exit
       write(21,150) stationid,rlon,rlat,rmdb,rmda,rmsb,rmsa,stddb,stdda,stdsb,stdsa,iqcflg
     endif
      read(11,110,IOSTAT=iflag) tdatano,tdatano2,xmedb,xmeda,smadb,smada
 
       if(iflag /=0 ) exit
!      print *,xmedb,xmeda,smadb,smada
      read(11,120,IOSTAT=iflag) stdno,breakb1,breakb2,breake1,breake2 
       if(iflag /=0 ) exit
!       print *,ntdata,breakb1,breakb2,breake1,breake2
       ncount=ncount+1
       
     enddo   loopd

      
100 format(2x,a8,6f8.2,i3)
150 format(2x,a8,10f8.2,i3)
110 format(2x,6f8.2)
120 format(2x,5f8.2)

    print *,' read_list2_sfc:ncount= ',ncount


!    do i=1,ncount
!        write(6,200) stdid(i),cid(i,1),cid(i,2),rmean(i,1),rmean(i,2),&
!                     std(i,1),std(i,2),break(i,1),break(i,2),std_no(i)
!      enddo
!
!200  format(a8,9f7.2)


       return
       end
  
