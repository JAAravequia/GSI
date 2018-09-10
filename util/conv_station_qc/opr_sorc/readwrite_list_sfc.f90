!    read the bias and rejection list

     
     subroutine readwrite_list_sfc(dtype,filetype,nd,itype)
    
    
     character(10) dtype,ddtype,filetype
     character(8) stationid
     character(50) file,file2
     real(4) rlon,rlat,stdno,rmeanb,rmeana,stdb,stda,tdatano
     real(4) rmdb,rmda,rmsb,rmsa,stddb,stdda,stdsb,stdsa
     real(4) tdatano2,xmedb,xmeda,smadb,smada,breakb1,breakb2,breake1,breake2
     integer n_tdata
     


    print *,'readwrite_list_sfc:',dtype,filetype,nd,itype
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

      print *,'file,file2=',file,file2

     open (11,file=file,form='formatted')
     open (21,file=file2,form='formatted')
     

  loopd: do

    if( nd ==2) then
       read(11,100,IOSTAT=iflag) stationid,i,rlon,rlat,stdno,rmeanb,rmeana,stdb,stda,tdatano,tdatano2,iqcflg
       if(iflag /=0 ) exit
!       print *, stationid,rlon,rlat,stdno,rmeanb,rmeana,stdb,stda,tdatano,tdatano2
        write(21,130) stationid,rlon,rlat,rmeanb,rmeana,stdb,stda,iqcflg
        read(11,110,IOSTAT=iflag) xmedb,xmeda,smadb,smada
        read(11,120,IOSTAT=iflag) n_tdata,breakb1,breakb2,breake1,breake2       
       ncount=ncount+1
     else if(nd ==4) then
       read(11,105,IOSTAT=iflag) stationid,i,rlon,rlat,istdno,rmdb,rmsb,stddb,stdsb,tdatano,tdatano2,iqcflg
       if(iflag /=0 ) exit
       read(11,115,IOSTAT=iflag) xmedb,xmeda,smadb,smada,rmda,rmsa,stdda,stdsa
       write(21,135) stationid,rlon,rlat,rmdb,rmda,rmsb,rmsa,stddb,stdda,stdsb,stdsa,iqcflg
       read(11,125,IOSTAT=iflag) n_tdata,breakb1,breakb2,breake1,breake2
       ncount=ncount+1
     endif
 
!      print *,xmedb,xmeda,smadb,smada
!       print *,ntdata,breakb1,breakb2,breake1,breake2
       
     enddo   loopd

      
100 format(a8,i8,9f8.2,i3)
110 format(16x,4f8.2)
120 format(16x,i8,4f8.2)
105 format(3x,a8,i8,9f8.2,i3)
115 format(3x,8f8.2)
125 format(3x,i8,4f8.2)

130 format(2x,a8,6f8.2,i3)
135 format(2x,a8,10f8.2,i3)



    print *,' read_list_sfc:ncount= ',ncount


!    do i=1,ncount
!        write(6,200) stdid(i),cid(i,1),cid(i,2),rmean(i,1),rmean(i,2),&
!                     std(i,1),std(i,2),break(i,1),break(i,2),std_no(i)
!      enddo
!
!200  format(a8,9f7.2)

       close(21)
       close(11)

       return
       end
  
