!  the program to read prepbufr rawinsonde data

   subroutine read_prepbufr_sfc_w_mor(sub,dtype,rtype,gross)


   real(4), dimension(37000,6) :: cid
!   real(8),dimension(5) :: qms,bac,rcs,ans,pcs
!   real(8),dimension(8) :: vqc
   real(8),dimension(4) :: obs
   real(8),dimension(7) :: hdr
   real(8),dimension(8) :: all
   real(8) rstationid,gross

!   character(len=50) :: hdstr,obstr,qmstr,qcstr,rcstr,anstr,bacstr,vqcstr,esbstr,pcstr
   character(len=50) :: hdstr,obstr
   character(len=60) :: allstr
   character(len=8),dimension(137000) ::  stdid
   character(8) subset,stationid,sub,cdtype
   character*5 dtype
   real rtype
   integer itype,iuse
   
   

    real(8),dimension(137000,14) ::  w220
    real(4),DIMENSION(137000,2) :: w220_no


  data hdstr  /'SID XOB YOB ELV T29 ITP TYP '/
   data allstr /'WQM UAN VAN UFC VFC WVWTG WVWTA WOE'/ 
   data obstr /'UOB VOB WPC WRC '/
   
  data lunin /11/

  equivalence(rstationid,stationid)


  nread=0
  ndata=0
  w220=0.0
  w220_no=0.0
  call closbf(lunin)
   open(lunin,file='prepbufr.post',form='unformatted')
   call openbf(lunin,'IN',lunin)
   do while(ireadmg(lunin,subset,idate).eq.0)
      if(SUBSET.NE. sub ) cycle 
      do while(ireadsb(lunin).eq.0)
         call ufbint(lunin,hdr,7,1,iret,hdstr)   ! read header from one observation
         if(hdr(7) /= rtype ) cycle
!       write(6,150) (hdr(i),i=1,7)
!150 format(7f10.1)
!      call ufbint(lunin,obs,6,1,iret,obstr) ! read the observation elements
!      call ufbint(lunin,qms,5,1,iret,qcstr) ! read the current quality marks
!      call ufbint(lunin,rcs,5,1,iret,rcstr) ! read the data with reason code
!      call ufbint(lunin,ans,5,1,iret,anstr) ! read the analysis
!      call ufbint(lunin,bac,5,1,iret,bacstr) ! read back ground
!      call ufbint(lunin,pcs,5,1,iret,pcstr) ! read back ground
!      call ufbint(lunin,vqc,8,1,iret,vqcstr) ! read variational qc weight
         call ufbint(lunin,all,8,1,iret,allstr)   !  read all the data
         call ufbint(lunin,obs,4,1,iret,obstr)  !  read different
         if(hdr(2) <0.0) hdr(2)=360.00+hdr(2)
       if(hdr(2) >360.00) then
         print *,'read_prepbufr_sfc_w_mor:problem with longitudedtype=',dtype,hdr(1),hdr(2),hdr(3)
         cycle
        endif
       if(abs(hdr(3)) >90.0) then
         print *,'read_prepbufr_sfc_w_mor:problem with latitude, dtype=',dtype,hdr(1),hdr(2),hdr(3)
         cycle
        endif

         if(all(3) >20000.0) cycle
         if(hdr(7) == rtype .and. all(3) <20000.0) then
            nread=nread+1
            if(nread==1) then
               ndata=ndata+1
               rstationid=hdr(1)
               stdid(ndata)=stationid 
               cid(ndata,1:6)=hdr(2:7) 
            else
               do i=ndata,1,-1
                  rstationid=hdr(1)
                  if(stationid == stdid(i))then
                     exit
                  else if( i == 1) then
                     ndata=ndata+1
                     rstationid=hdr(1)
                     stdid(ndata)=stationid
                     cid(ndata,1:6)=hdr(2:7)
                  endif
               enddo
            endif
            if(nread==2) then
               write(6,250) stationid
               write(6,200) obs(1),obs(2),obs(3),obs(4),(all(i),i=1,8)
            endif
250 format(a8)
            if( all(8) <1000.0) then
               if( all(1)  <0.0 .or. all(1) ==10.0) then
                  w220_no(ndata,2)=w220_no(ndata,2)+1.0
               else if(obs(1) <2000.0 .and. all(2) <2000.0 .and. all(4) <2000.0) then 
                  obu=obs(1)
                  obv=obs(2)
                  bacu=all(4)
                  bacv=all(5)
                  ansu=all(2)
                  ansv=all(3)
                  call  UV2DS(obu,obv)   ! obu: wind direction,obv: wind speed
                  call  UV2DS(bacu,bacv)
                  call  UV2DS(ansu,ansv)
                  if(obsu ==360.0) obsu=bacu
                  if(bacu == 360.0) bacu=obsu
                  if(ansu == 360.0) ansu=obsu
                  dif=obu-bacu
                  if(dif >180.0) dif=360.0-dif
                  if(dif <-180.0) dif=-360-dif
                  difa=obu-ansu
                  if(difa >180.0) difa=360.0-difa
                  if(difa <-180.0) difa=-360-difa
                  diff=(obs(1)-all(4))**2+(obs(2)-all(5))**2
                  diff=sqrt(diff)
                  ratio=ratio/all(8)
                  if(ratio >gross) then
                     w220_no(ndata,2)=w220_no(ndata,2)+1.0
                  else
                     w220_no(ndata,1)=w220_no(ndata,1)+1.0
! for u and v
                     w220(ndata,1)=w220(ndata,1)+obs(1)-all(4)   ! obs-back(u)
                     w220(ndata,2)=w220(ndata,2)+obs(1)-all(2)   ! obs-anl(u)
                     w220(ndata,3)=w220(ndata,3)+all(6)             ! variational qc weight(bac)
                     w220(ndata,4)=w220(ndata,4)+all(7)             ! variational qc weight(bac)
                     w220(ndata,5)=w220(ndata,5)+obs(2)-all(5)   ! obs-back(v)
                     w220(ndata,6)=w220(ndata,6)+obs(2)-all(3)   ! obs-anl(v)
                     w220(ndata,7)=w220(ndata,7)+obs(1)             ! obs u
                     w220(ndata,8)=w220(ndata,8)+obs(2)             ! obs v
!  for wind speed and direction
                     w220(ndata,9)=w220(ndata,9)+dif
                     w220(ndata,10)=w220(ndata,10)+difa
                     w220(ndata,11)=w220(ndata,11)+obv-bacv  !o-b speed
                     w220(ndata,12)=w220(ndata,12)+obv-ansv  !o-a speed
                     w220(ndata,13)=w220(ndata,13)+obu      ! obs direction
                     w220(ndata,14)=w220(ndata,14)+obv      ! obs speed
                  endif
               endif 
            else
               if( all(1) <0.0 .or. all(1) ==10.0) then
                  w220_no(ndata,2)=w220_no(ndata,2)+1.0
               else if(obs(1) <2000.0 .and. all(2) <2000.0 .and. all(4) <2000.0) then
                  obu=obs(1)
                  obv=obs(2)
                  bacu=all(4)
                  bacv=all(5)
                  ansu=all(2)
                  ansv=all(3)
                  call  UV2DS(obu,obv)   ! obu: wind direction,obv: wind speed
                  call  UV2DS(bacu,bacv)
                  call  UV2DS(ansu,ansv)
                  if(obsu ==360.0) obsu=bacu
                  if(bacu == 360.0) bacu=obsu
                  if(ansu == 360.0) ansu=obsu
                  dif=obu-bacu
                  if(dif >180.0) dif=360.0-dif
                  if(dif <-180.0) dif=-360-dif
                  difa=obu-ansu
                  if(difa >180.0) difa=360.0-difa
                  if(difa <-180.0) difa=-360-difa
                  diff=(obs(1)-all(4))**2+(obs(2)-all(5))**2
                  diff=sqrt(diff)
                  ratio=diff/2.0
                  if(ratio >gross) then
                     w220_no(ndata,2)=w220_no(ndata,2)+1.0
                  else
                     w220_no(ndata,1)=w220_no(ndata,1)+1.0
! for u and v
                     w220(ndata,1)=w220(ndata,1)+obs(1)-all(4)   ! obs-back(u)
                     w220(ndata,2)=w220(ndata,2)+obs(1)-all(2)   ! obs-anl(u)
                     w220(ndata,3)=w220(ndata,3)+all(6)             ! variational qc weight(bac)
                     w220(ndata,4)=w220(ndata,4)+all(7)             ! variational qc weight(bac)
                     w220(ndata,5)=w220(ndata,5)+obs(2)-all(5)   ! obs-back(v)
                     w220(ndata,6)=w220(ndata,6)+obs(2)-all(3)   ! obs-anl(v)
                     w220(ndata,7)=w220(ndata,7)+obs(1)             ! obs u
                     w220(ndata,8)=w220(ndata,8)+obs(2)             ! obs v
!  for wind speed and direction
                     w220(ndata,9)=w220(ndata,9)+dif
                     w220(ndata,10)=w220(ndata,10)+difa
                     w220(ndata,11)=w220(ndata,11)+obv-bacv  !o-b speed
                     w220(ndata,12)=w220(ndata,12)+obv-ansv  !o-a speed
                     w220(ndata,13)=w220(ndata,13)+obu      ! obs direction
                     w220(ndata,14)=w220(ndata,14)+obv      ! obs speed
                  endif
               endif
            endif
       endif      !!  for type screen

     enddo      !! enddo ireadsub
   enddo      !! enddo  ireadmg 

           print *,'read_prepbufr_sfc_w ',dtype,nread,ndata

!           write(6,100) (stdid(i),i=1,ndata)
!100 format(8a8)

               do i=1,ndata
                if(w220_no(i,1) >1.0) then
                  do j=1,14
                    w220(i,j)=w220(i,j)/w220_no(i,1)
                  enddo
                else if(w220_no(i,1) ==0.0) then
                   do j=1,14
                    w220(i,j)=-999.0
                  enddo
                endif
             enddo

             

           write(6,*) 'wind'
           write(6,200) w220_no(1,1),w220_no(1,2) 
           write(6,200) (w220(1,i),i=1,14) 
           write(6,200) (w220_no(10,i),i=1,2) 
           write(6,200) (w220(10,i),i=1,14) 
200 format(10f7.2)

          dtype=trim(dtype)
       
        if(ndata >0) then

         open(40,file=dtype,form='unformatted')
           write(40) ndata
           write(40) stdid(1:ndata),cid(1:ndata,1:6),w220_no(1:ndata,1:2),w220(1:ndata,1:14)
           close(40)
        endif 


          close(11)
          return 
          end     
   

 
