!   the program read the data 
!   the program read out put from read_prepbufr_***, which put the data into
!   each station and type.
!  plevel: hold vertical level values
!  rcid and cid to get station infomation: 1: logitude,2 latitude
!  3: station elevation, 4: DATA DUMP REPORT TYPE
!  5: instrument type, 6: EPBUFR REPORT TYPE
!  rdata: d1: station number, d2, station statistics:
!  for non wind tyep:  1: O-B,2: O-A,3:  variational weight for first
!  inner loop, 4: variational qc weight for final weight, 5: observation.  
!  For wind type: 1: O-B (u), 2: O-A(u),3: variational weight for first inner
!  loop,
!  4: variational qc weight for final weight, 5: O-B(v), 6: O-A (v),
!  7: u observation,8: v observation,9: O-B (direction),10:  O-A (direction)
!  11: O-B (speed), 12: O-A (speed),13: direction observation, 14: speed
!  observation
!  stdid and rstdid:  station ID
!  tdata: d1: station number, d2, station statistics:
!  for non wind tyep:  1: O-B,2: O-A,3:  variational weight for first
!  inner loop, 4: variational qc weight for final weight, 5: observation.  
!  For wind type: 1: O-B (u), 2: O-A(u),3: variational weight for first inner
!  loop,
!  4: variational qc weight for final weight, 5: O-B(v), 6: O-A (v),
!  7: u observation,8: v observation,9: O-B (direction),10:  O-A (direction)
!  11: O-B (speed), 12: O-A (speed),13: direction observation, 14: speed
!  d3: number in time (how many cycles) 

!  rdata_no and tdata_no:  d1: station number, d2: data
!  count, 1: used data counter in GSI, 2:  rejected by variaous qc.  
!  For tdata_no: d3: number in time  


    subroutine read_data_sfc(dtype,n)

    real(4),allocatable,dimension(:,:) :: rcid 
    real(8),allocatable,dimension(:,:) :: rdata
    real(4),allocatable,dimension(:,:) :: rdata_no
    
    character(8),allocatable,dimension(:) :: rstdid 

    character(len=8),dimension(75000) ::  stdid
    real(4), dimension(75000,6) :: cid
    real(4), dimension(75000,6,400) :: cid_time           !! for non fixed station 
    real(4),dimension(75000,n,400) :: tdata
    real(4),dimension(75000,2,400) :: tdata_no

    character(5) dtype
    character*1 ddtype
 
    
    character(8)  stid 

    character(120) cfile
    character(50) fname,fileout

    real rmiss

 data rmiss/-999.0/ 


     print *, dtype,'n=',n,nk
     ddtype=dtype(1:1)
        ddtype=trim(ddtype)
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
    read(21,'(a120)',IOSTAT=iflag) cfile
    if( iflag /= 0 ) exit loopd
    print *, 'read the file ',cfile
    open(unit=11,file=cfile,form='unformatted',IOSTAT=iflag2)
    if( iflag2 /= 0 ) exit loopd
    rewind(11)
    nread=nread+1
    ndata=0
    read(11) ndata
    print *, ndata
    allocate(rcid(ndata,6),rdata(ndata,n),rstdid(ndata),rdata_no(ndata,2),stat=istatus)
    if (istatus /= 0) then
      print *,'readdata_sfc: cannot allocate the array'
    endif
    read(11) rstdid,rcid,rdata_no,rdata

     print *, rstdid(1),rcid(1,1),rdata_no(1,1),rdata_no(1,2),rdata(1,1),rdata(1,2)

   if(nread == 1) then
      cid(1:ndata,:) = rcid(1:ndata,:)
      stdid(1:ndata)=rstdid(1:ndata)
      tdata_no(1:ndata,:,1)=rdata_no(1:ndata,:)
      tdata(1:ndata,:,1)=rdata(1:ndata,:)
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
              print *, 'READ_DATA_SFC:fixed station ',rstdid(j)
              print *,rcid(j,1),rcid(j,2),rcid(j,3),rcid(j,4),rcid(j,5)
              print *,cid(i,1),cid(i,2),cid(i,3),cid(i,4),cid(i,5)
            endif 
            do k=1,4
             if(rcid(j,k) == cid(i,k) ) then    !! for fix station to check lat. and lon.
               cycle
             else
               print *, 'READ_DATA_SFC:problem to match location and type',nread,rstdid(j)
               print *,rcid(j,1),rcid(j,2),rcid(j,3),rcid(j,4),rcid(j,5) 
               print *,cid(i,1),cid(i,2),cid(i,3),cid(i,4),cid(i,5) 
!               stop
                exit
             endif
            enddo
         else if(cid(i,4) ==  13.0) then
           if(rcid(j,1) /= cid(i,1) .and. rcid(j,2) /= cid(i,2) ) then
             print *,'READ_DATA_SFC:moveable station',stdid(i)
             print *,rcid(j,1),rcid(j,2),rcid(j,3),rcid(j,4),rcid(j,5)
             print *,cid(i,1),cid(i,2),cid(i,3),cid(i,4),cid(i,5)
           endif
           do k=1,2
             if( abs(rcid(j,k)-cid(i,k)) <30.0 ) then
              cycle
             else
              print *, 'READ_DATA_SFC: problem to match location and type',nread,rstdid(j)
              print *,rcid(j,1),rcid(j,2),rcid(j,3),rcid(j,4),rcid(j,5)
              print *,cid(i,1),cid(i,2),cid(i,3),cid(i,4),cid(i,5)
!              stop
             endif 
            enddo
         endif
          tdata_no(i,:,nread)=rdata_no(j,:)
          tdata(i,:,nread)=rdata(j,:)
          cid_time(i,:,nread)=rcid(j,:)
          exit
   else if( i== ndata1) then
          ndata2=ndata2+1
          stdid(ndata2)=rstdid(j)
          cid(ndata2,:)=rcid(j,:)
          tdata_no(ndata2,:,nread)=rdata_no(j,:)
          tdata(ndata12,:,nread)=rdata(j,:)
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

     fileout=trim(dtype)//'_out'

      open(30,file=fileout,form='unformatted')

      write(30) ndata1,nread,n
    write(30) stdid(1:ndata1),cid(1:ndata1,:)
    write(30) cid_time(1:ndata1,:,1:nread)
    write(30) tdata(1:ndata1,:,1:nread)
    write(30) tdata_no(1:ndata1,:,1:nread)

  if(ddtype == 'u') then
    write(6,1000) (tdata(11,1,i),i=1,10)
    write(6,1000) (tdata(12,1,i),i=1,10)
    write(6,1000) (tdata(13,2,i),i=1,10)
    write(6,1000) (tdata(14,3,i),i=1,10)
  endif

1000 format(10f8.3)

!     if (trim(dtype) == 'ps120' ) then

        write(6,5000)(tdata_no(10,1,iread),iread=1,10)
        write(6,5000)(tdata_no(10,2,iread),iread=1,10)

!   endif

5000 format(10f8.1)
     print *,'READ_DATA_SFC:',dtype,ndata1,nread

    return 
   end
