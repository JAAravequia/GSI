!    read the bias and rejection list

     
     subroutine read_list2_sfc(dtype,file,ndata,ncount,nd,nd2,stdid,cid,rmean,std,xmed,break,std_no,tdata_no,iqcflg)
    
    
     character(5) dtype
     character(8) stationid
     character(50) file
     character(8),dimension(ndata) :: stdid
     real(4),dimension(ndata) :: std_no
     real(4),dimension(ndata,2) :: cid,tdata_no
     real(4),dimension(ndata,nd) :: rmean,std
     real(4),dimension(ndata,nd2) :: xmed,break 
     real(4) rlon,rlat,stdno,rmeanb,rmeana,stdb,stda,tdatano
     real(4) tdatano2,xmedb,xmeda,smadb,smada,breakb1,breakb2,breake1,breake2
     integer,dimension(ndata) :: iqcflg
     logical lexist
     


    Print *,' read_list2_sfc: ndata=',ndata
     ncount=0
     inquire ( file=file,exist=lexist)
     if(.not. lexist) then
       print *,'read_list2_sfc:the file doesnot exist ',file
       return
     endif
     open (11,file=file,form='formatted')

     loopd: do

      read(11,100,IOSTAT=iflag) stationid,i,rlon,rlat,stdno,rmeanb,rmeana,&
                                stdb,stda,tdatano,tdatano2,iqc
       if(iflag /=0 ) exit
!       print *, stationid,rlon,rlat,stdno,rmeanb,rmeana,stdb,stda,tdatano,tdatano2,iqc
      read(11,110,IOSTAT=iflag) xmedb,xmeda,smadb,smada
       if(iflag /=0 ) exit
!      print *,xmedb,xmeda,smadb,smada
      read(11,120,IOSTAT=iflag) ntdata,breakb1,breakb2,breake1,breake2 
       if(iflag /=0 ) exit
!       print *,ntdata,breakb1,breakb2,breake1,breake2
       ncount=ncount+1

!       print *,ncount
       stdid(ncount)=stationid
       cid(ncount,1)=rlon
       cid(ncount,2)=rlat
       rmean(ncount,1)=rmeanb
       rmean(ncount,2)=rmeana
       std(ncount,1)=stdb
       std(ncount,2)=stda
       xmed(ncount,1)=xmedb
       xmed(ncount,2)=xmeda
       xmed(ncount,3)=smadb
       xmed(ncount,4)=smada
       break(ncount,1)=breakb1
       break(ncount,2)=breakb2
       break(ncount,3)=breake1
       break(ncount,4)=breake2
       std_no(ncount) = stdno
       tdata_no(ncount,1)= tdatano
       tdata_no(ncount,2)= tdatano2
       iqcflg(ncount)=iqc

     enddo   loopd

      

100 format(a8,i8,9f8.2,i3)
110 format(16x,4f8.2)
120 format(16x,i8,4f8.2)

    print *,' read_list2_sfc:ncount= ',ncount,iqcflg(4)


!    do i=1,ncount
!        write(6,200) stdid(i),cid(i,1),cid(i,2),rmean(i,1),rmean(i,2),&
!                     std(i,1),std(i,2),break(i,1),break(i,2),std_no(i)
!      enddo
!
!200  format(a8,9f7.2)


       return
       end
  
