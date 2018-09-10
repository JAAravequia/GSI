!    read the bias and rejection list

     
     subroutine read_list2formake(dtype,file,nd,ndata,ncount,stdid)
    
    
     character(5) dtype
     character(8) stationid
     character(50) file
     character(8),dimension(ndata) :: stdid
     real(4) rlon,rlat,stdno,rmeanb,rmeana,stdb,stda,tdatano
     real(4) tdatano2,xmedb,xmeda,smadb,smada,breakb1,breakb2,breake1,breake2
     logical lexist     


    Print *,' read_list2: ndata=',ndata

 ncount =0

   inquire (file=file,exist=lexist)
     if(.not. lexist) then
       print *,'read_list2:the file doesnot exist ',file
       return
     endif

   open (11,file=file,form='formatted',iostat=ierror)
   if(ierror ==0) then

     loopd: do
      read(11,100,IOSTAT=iflag) stationid,rlon,rlat,nlev 
      if(iflag /=0 ) exit
       ncount=ncount+1
       stdid(ncount)=stationid
       do k=1, nlev
         if(nd ==2) then
           read(11,120) plve1,plve2,rmeanb,rmeana,stdb,stda
         else if (nd ==4) then
           read(11,120) plve1,plve2,rmeanbw,rmeanaw,rmeanbs,rmeanas,stdbw,&
                        stdaw,stdbs,stdas
         endif
       enddo

     enddo   loopd

 endif      

100 format(2x,a8,2f8.2,i8)
110 format(2x,6f8.2)
120 format(2x,10f8.2)

    print *,' read_list2formake:ncount= ',ncount
    print *,' file= ',file


       return
       end
 
