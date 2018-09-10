!   the program read the data 


    real(4),dimension(43) :: plevel
    real(4),allocatable,dimension(:,:) :: rcid,tmp 
    real(4),allocatable,dimension(:,:,:) :: rdata,rdata_no
    real(4),allocatable,dimension(:) :: tmp2
    
    character(8),allocatable,dimension(:) :: rstdid 

    character(len=8),dimension(2000) ::  stdid
    real(4), dimension(5000,6) :: cid
    real(4), dimension(5000,6,400) :: cid_time           !! for non fixed station 
    real(8),dimension(5000,43,14,400) :: tdata
    real(8),dimension(5000,43,2,400) :: tdata_no

    character(5) dtype
    
    character(8)  stid 

    character(100) cfile
    character(50) fname,fileout

    real rmiss

  data plevel/1000.,975.,950.,925.,900.,875.,850.,825.,800.,775.,750.,725.,700.,675.,650.,625.,600.,575.,550.,525.,500.,475.,450.,425.,400.,375.,350.,325.,300.,275.,250.,225.,200.,175.,150.,125.,100.,70.,50.,30.,20.,10.,5./

! data dtype /'t120'/
 data rmiss/-999.0/ 

    namelist /input/dtype

    read(5,input)
    write(6,*)' User input below'
    write(6,input)
    nread=0
    nmatch=0
    tdata_no=0.0
    tdata=rmiss
  rtim=0.0
  nflg0=0
  xlat0=0.0
  xlon0=0.0
  nlev0=0
  stid='        '


 open(21,file='fname')
 loopd: do
    read(21,'(a100)',IOSTAT=iflag) cfile
    if( iflag /= 0 ) exit loopd
    print *, 'read the file ',cfile
    open(unit=11,file=cfile,form='unformatted',IOSTAT=iflag2)
    if( iflag2 /= 0 ) exit loopd
    rewind(11)
    nread=nread+1
    read(11) ndata
    allocate(rcid(ndata,6),rdata(ndata,43,14),rstdid(ndata),rdata_no(ndata,43,2))
    read(11) rstdid,rcid,rdata_no,rdata

   if(nread == 1) then
      cid(1:ndata,:) = rcid(1:ndata,:)
      stdid(1:ndata)=rstdid(1:ndata)
      tdata_no(1:ndata,:,:,1)=rdata_no(1:ndata,:,:)
      tdata(1:ndata,:,:,1)=rdata(1:ndata,:,:)
      cid_time(1:ndata,:,1)=rcid(1:ndata,:)
      ndata1=ndata
     deallocate(rcid,rdata,rstdid,rdata_no)
   else
     nmatch=0
     ndata2=ndata1
     do j=1,ndata
     do i=1,ndata1
        if(rstdid(j) == stdid(i) ) then
          nmatch=nmatch+1   
          if(cid(i,4) == 11.0) then
            if(rcid(j,5) /=cid(i,5)) then
              print *, 'fixed station'
              print *,rcid(j,1),rcid(j,2),rcid(j,3),rcid(j,4),rcid(j,5),rstdid(j)
              print *,cid(i,1),cid(i,2),cid(i,3),cid(i,4),cid(i,5),stdid(i)
            endif 
            do k=1,4
             if(rcid(j,k) == cid(i,k) ) then    !! for fix station to check lat. and lon.
               cycle
             else
               print *, 'there is problem to match location and type',nread
               print *,rcid(j,1),rcid(j,2),rcid(j,3),rcid(j,4),rcid(j,5),rstdid(j) 
               print *,cid(i,1),cid(i,2),cid(i,3),cid(i,4),cid(i,5),stdid(i) 
!               stop
                exit
             endif
            enddo
         else if(cid(i,4) ==  13.0) then
           if(rcid(j,1) /= cid(i,1) .and. rcid(j,2) /= cid(i,2) ) then
             print *,'moveable station'
             print *,rcid(j,1),rcid(j,2),rcid(j,3),rcid(j,4),rcid(j,5),rstdid(j)
             print *,cid(i,1),cid(i,2),cid(i,3),cid(i,4),cid(i,5),stdid(i)
           endif
           do k=1,2
             if( abs(rcid(j,k)-cid(i,k)) <30.0 ) then
              cycle
             else
              print *, 'there is problem to match location and type',nread
              print *,rcid(j,1),rcid(j,2),rcid(j,3),rcid(j,4),rcid(j,5),rstdid(j)
              print *,cid(i,1),cid(i,2),cid(i,3),cid(i,4),cid(i,5),stdid(i)
              stop
             endif 
            enddo
         endif
          tdata_no(i,:,:,nread)=rdata_no(j,:,:)
          tdata(i,:,:,nread)=rdata(j,:,:)
          cid_time(i,:,nread)=rcid(j,:)
          exit
   else if( i== ndata1) then
          ndata2=ndata2+1
          stdid(ndata2)=rstdid(j)
          cid(ndata2,:)=rcid(j,:)
          tdata_no(ndata2,:,:,nread)=rdata_no(j,:,:)
          tdata(ndata12,:,:,nread)=rdata(j,:,:)
          cid_time(ndata2,:,nread)=rcid(j,:)
       endif
      enddo
      enddo
      ndata1=ndata2
      deallocate(rcid,rdata,rstdid,rdata_no)
      print *, nmatch,ndata1,ndata2,ndata
   endif
        
  enddo loopd          

   print *,nread

     fileout=trim(dtype)//'.out'

      open(30,file=fileout,form='unformatted')

      write(30) ndata1,nread
    write(30) stdid(1:ndata1),cid(1:ndata1,:)
    write(30) cid_time(1:ndata1,:,1:nread)
    write(30) tdata(1:ndata1,:,:,1:nread)
    write(30) tdata_no(1:ndata1,:,:,1:nread)


   stop
   end
    
   


    
  
