!    read the bias and rejection list

     
     subroutine read_list2(dtype,file,ndata,ncount,nd,nd2,nk,stdid,cid,rmean,std,xmed,break,std_no,tdata_no,iqcflg,kklev)
    
    
     character(5) dtype
     character(8) stationid
     character(50) file
     character(8),dimension(ndata) :: stdid
     integer,dimension(ndata) ::  kklev
     real(4),dimension(ndata,2) :: cid
     real(4),dimension(ndata,nk) :: std_no
     real(4),dimension(ndata,nk,2) :: tdata_no
     real(4),dimension(ndata,nk,nd) :: rmean,std
     real(4),dimension(ndata,nk,nd2) :: xmed,break 
     real(4) rlon,rlat,stdno,rmeanb,rmeana,stdb,stda,tdatano
     real(4) tdatano2,xmedb,xmeda,smadb,smada,breakb1,breakb2,breake1,breake2
     integer,dimension(ndata,nk) :: iqcflg
     
     logical lexist

    Print *,' read_list2: ndata=',ndata

    std_no=-999.0
    tdata_no=-999.0
    rmean=-999.0
    std=-999.0
    xmed=-999.0
    break=-999.0
    ncount=0
    iqcflg=-999.0

     inquire (file=file,exist=lexist)
     if(.not. lexist) then
       print *,'read_list2:the file doesnot exist ',file
       return
     endif
     open (11,file=file,form='formatted')

     loopd: do

      read(11,105,IOSTAT=iflag) stationid,i,rlon,rlat,rlev
      if(iflag /=0 ) exit
      nlev=nint(rlev)
       ncount=ncount+1
       stdid(ncount)=stationid
       cid(ncount,1)=rlon
       cid(ncount,2)=rlat       
       kklev(ncount)=nlev
       do k=1, nlev
          read(11,100) stdno,rmeanb,rmeana,stdb,stda,tdatano,tdatano2,iqc
          read(11,110,IOSTAT=iflag) klev,xmedb,xmeda,smadb,smada
          read(11,120,IOSTAT=iflag) breakb1,breakb2,breake1,breake2 
          rmean(ncount,klev,1)=rmeanb
          rmean(ncount,klev,2)=rmeana
          std(ncount,klev,1)=stdb
          std(ncount,klev,2)=stda
          xmed(ncount,klev,1)=xmedb
          xmed(ncount,klev,2)=xmeda
          xmed(ncount,klev,3)=smadb
          xmed(ncount,klev,4)=smada
          break(ncount,klev,1)=breakb1
          break(ncount,klev,2)=breakb2
          break(ncount,klev,3)=breake1
          break(ncount,klev,4)=breake2
          std_no(ncount,klev) = stdno
          tdata_no(ncount,klev,1)= tdatano
          tdata_no(ncount,klev,2)= tdatano2
          iqcflg(ncount,klev)=iqc
       enddo

     enddo   loopd

      

105 format(2x,a8,i8,3f8.2)
100 format(2x,7f8.2,i3)
110 format(8x,i8,8f8.2)
120 format(8x,4f8.2)


    print *,' read_list2:ncount= ',ncount


    do i=1,ncount
       write(6,200) stdid(i),cid(i,1),cid(i,2),kklev(i)
       do k=1,nk
         if(rmean(i,k,1) >-900.0) then
            write(6,210) k,rmean(i,k,1),rmean(i,k,2),&
                     std(i,k,1),std(i,k,2),break(i,k,1),break(i,k,2),std_no(i,k)
         endif
      enddo
   enddo

200  format(a8,2f7.2,i8)
210  format(i8,7f7.2)


       return
       end
 
