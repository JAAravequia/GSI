!    read the bias and rejection list

     
     subroutine read_listw_sfcformake(dtype,file,ndata,ncount,stdid)
    
    
     character(5) dtype
     character(8) stationid
     character(50) file
     character(8),dimension(ndata) :: stdid
     real(4) rlon,rlat,stdno,rmeanb,rmeana,stdb,stda,tdatano
     real(4) tdatano2,xmedb,xmeda,smadb,smada,breakb1,breakb2,breake1,breake2
     


    Print *,' read_listw_sfc: ndata=',ndata
    ncount=0

     open (11,file=file,form='formatted',iostat=ierror)
      if(ierror ==0) then

     loopd: do

      read(11,100,IOSTAT=iflag) stationid,i,rlon,rlat,stdno,rmeanbw,rmeanbs,&
                                stdbw,stdbs,tdatano,tdatano2
       if(iflag /=0 ) exit
      read(11,110,IOSTAT=iflag) xmedb,xmeda,smadb,smada,rmeanaw,rmeanas,stdaw,stdas
       if(iflag /=0 ) exit
      read(11,120,IOSTAT=iflag) ntdata,breakb1,breakb2,breake1,breake2 
       ncount=ncount+1

       stdid(ncount)=stationid

     enddo   loopd

     endif 

100 format(3x,a8,i8,9f8.2)
110 format(3x,8f8.2)
120 format(3x,i8,4f8.2)
    print *,' read_listw_sfc:ncount= ',ncount
    print *,' file= ',file



       return
       end
  
