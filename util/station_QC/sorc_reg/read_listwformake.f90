!    read the bias and rejection list

     
     subroutine read_listwformake(dtype,file,ndata,ncount,stdid)
    
    
     character(5) dtype
     character(8) stationid
     character(50) file
     character(8),dimension(ndata) :: stdid
     real(4) rlon,rlat,stdno,rmeanbw,rmeanaw,rmeanas,rmeanbs,tdatano
     real(4) stdbw,stdaw,stdas,stdbs
     real(4) tdatano2,xmedb,xmeda,smadb,smada,breakb1,breakb2,breake1,breake2
     logical lexist     


    Print *,' read_listw: ndata=',ndata

    std_no=-999.0
    tdata_no=-999.0
    rmean=-999.0
    std=-999.0
    xmed=-999.0
    break=-999.0
    ncount=0

     inquire (file=file,exist=lexist)
     if(.not. lexist) then
       print *,'read_listwformake:the file doesnot exist ',file
       return
     endif

     open (11,file=file,form='formatted',iostat=ierror)
     if(ierror ==0) then

     loopd: do

      read(11,105,IOSTAT=iflag) stationid,i,rlon,rlat,rlev
      if(iflag /=0 ) exit
      nlev=nint(rlev)
       ncount=ncount+1
       stdid(ncount)=stationid
       do k=1, nlev
          read(11,100) stdno,rmeanbw,rmeanbs,stdbw,stdbs,tdatano,tdatano2,iqc
          read(11,110) klev,xmedb,xmeda,smadb,smada
          read(11,120) breakb1,breakb2,breaka1,breaka2,rmeanaw,rmeanas,stdaw,stdas 
       enddo

     enddo   loopd

     endif 

105 format(2x,a8,i8,3f8.2)
100 format(2x,7f8.2,i3)
110 format(2x,i8,4f8.2)
120 format(2x,8f8.2)

    print *,' read_listw_sfc:ncount= ',ncount


       return
       end
  
