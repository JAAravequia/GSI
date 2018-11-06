!    read the bias and rejection list

     
     subroutine read_list2_sfcformake(dtype,file,nd,ndata,ncount,stdid)
    
    
     character(5) dtype
     character(8) stationid
     character(50) file
     character(8),dimension(ndata) :: stdid
     logical lexist 


    Print *,' read_list2_sfc: ndata=',ndata
     ncount=0
     inquire (file=file,exist=lexist)
     if(.not. lexist) then
       print *,'read_list2_sfc_formake:the file doesnot exist ',file
       return
     endif

     open (11,file=file,form='formatted',iostat=ierror)
  
     if(ierror ==0) then

     loopd: do

      if( nd ==2) then
         read(11,100,IOSTAT=iflag) stationid,rlon,rlat,rmeanb,rmeana,stdb,stda,iqc
      else if( nd ==4) then
         read(11,110,IOSTAT=iflag) stationid,rlon,rlat,rmdb,rmda,rmsb,rmsa,&
                                   stddb,stdda,stdsb,stdsa,iqc 
      endif

       if(iflag /=0 ) exit
       ncount=ncount+1
       stdid(ncount)=stationid
     enddo   loopd
    endif
      

100 format(2x,a8,6f8.2,i3)
110 format(2x,a8,10f8.2,i3)

    print *,' file= ',file
    print *,' read_list2_sfc:ncount= ',ncount

!      do i=1,ncount
!         write(6,200) i,stdid(i)
!       enddo 
!
!200 format(i8,3x,a8)

       return
       end
  
