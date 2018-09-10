!  the program to read prepbufr rawinsonde data

   subroutine read_prepbufr_wind_mor(sub,dtype,rtype,gross)


   real(4), dimension(2000,6) :: cid
   real(8),dimension(255) :: qms,rcs,pcs,obserr
   real(8),dimension(2,255) :: vqc,bac,ans
   real(8),dimension(3,255) :: obs
!   real(8),dimension(8,255) :: all
   real(8),dimension(7) :: hdr
   real(8) rstationid,gross

   character(len=50) :: hdstr,obstr,qmstr,qcstr,rcstr,anstr,bacstr,vqcstr,esbstr,pcstr,oestr
!   character(len=50) :: hdstr,obstr
!   character(len=60) :: allstr
   character(len=8),dimension(2000) ::  stdid
   character(8) subset,stationid,sub,cdtype
   character*5 dtype
   real rtype
   integer itype,iuse
   
   

    real(8),dimension(2000,43,14) ::  w220
    real(4),DIMENSION(2000,43,2) :: w220_no

    real(4),dimension(43) :: plevel

  data plevel/1000.,975.,950.,925.,900.,875.,850.,825.,800.,775.,750.,725.,700.,675.,650.,625.,600.,575.,550.,525.,500.,475.,450.,425.,400.,375.,350.,325.,300.,275.,250.,225.,200.,175.,150.,125.,100.,70.,50.,30.,20.,10.,5./
  data hdstr  /'SID XOB YOB ELV T29 ITP TYP'/
!  data obstr  /'POB QOB TOB UOB VOB CAT ' /
!  data qcstr  /'PQM QQM TQM WQM ZQM '/
!  data anstr / 'PAN QAN TAN  UAN VAN '/
!  data rcstr / 'PRC QRC TRC WRC ZRC ' /
!  data bacstr / 'PFC QFC TFC UFC VFC ' /
!  data pcstr / 'PPC QPC TPC WPC ZPC '/
!  data oestr  /'POE QOE TOE WOE'/
!  data vqcstr / 'PVWTG QVWTG TVWTG WVWTG PVWTA QVWTA TVWTA WVWTA '/
   data obstr  /'POB UOB VOB ' /
  data qcstr  /'WQM '/
  data anstr / ' UAN VAN '/
  data rcstr / ' WRC  ' /
  data bacstr / ' UFC VFC ' /
  data pcstr / ' WPC  '/
  data oestr  /' WOE'/
  data vqcstr /'WVWTG  WVWTA'/
!  data vqcstr / 'PVWTG QVWTG TVWTG WVWTG PVWTA QVWTA TVWTA WVWTA '/
!  data esbstr  /'ESBAK'/
!  data allstr /'WQM UAN VAN UFC VFC WVWTG WVWTA WOE'/
!   data obstr /'POB UOB VOB WPC WRC '/
  data lunin /11/

  equivalence(rstationid,stationid)

!  call convinfo_read(dtype,gross)

     print *,sub,dtype, gross,rtype

  nread=0
  ndata=0
  w220=0.0
  w220_no=0.0
  call closbf(lunin)
   open(lunin,file='prepbufr.post',form='unformatted')
   call openbf(lunin,'IN',lunin)
   do while(ireadmg(lunin,subset,idate).eq.0)
   if(subset .NE. sub ) cycle 
   do while(ireadsb(lunin).eq.0)
     call ufbint(lunin,hdr,7,1,iret,hdstr)   ! read header from one observation
     if(hdr(7) /= rtype) cycle
     if( hdr(7) == rtype) then 
       nread=nread+1
!       print *, nread
      call ufbint(lunin,obs,3,255,nlev,obstr) ! read the observation elements
      call ufbint(lunin,qms,1,255,nlev,qcstr) ! read the current quality marks
      call ufbint(lunin,rcs,1,255,nlev,rcstr) ! read the data with reason code
      call ufbint(lunin,ans,2,255,nlev,anstr) ! read the analysis
      call ufbint(lunin,bac,2,255,nlev,bacstr) ! read back ground
      call ufbint(lunin,pcs,1,255,nlev,pcstr) ! read back ground
      call ufbint(lunin,vqc,2,255,nlev,vqcstr) ! read variational qc weight
      call ufbint(lunin,obserr,1,255,nlev,oestr) ! read observation error
     
      if(hdr(2) <0.0) hdr(2)=360.00+hdr(2)
       if(hdr(2) >360.00) then
         print *,'read_prepbufr_wind_mor:problem with longitudedtype=',dtype,hdr(1),hdr(2),hdr(3)
         cycle
        endif
       if(abs(hdr(3)) >90.0) then
         print *,'read_prepbufr_wind_mor:problem with latitude, dtype=',dtype,hdr(1),hdr(2),hdr(3)
         cycle
        endif

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
         else
           if( i == 1) then
            ndata=ndata+1
            rstationid=hdr(1)
            stdid(ndata)=stationid
            cid(ndata,1:6)=hdr(2:7)
           endif
         endif
      enddo
     endif

!        print *,nlev
         nkk=0
         do kk=1,nlev
           do k=1,43
              if(k ==1) then
                plve1=plevel(k)+200.0
                plve2=plevel(k)-12.5
              else if( k <=37) then
                plve1=plevel(k)+12.5
                plve2=plevel(k)-12.5
              else if( k == 38) then
                plve1=plevel(k)+17.5
                plve2=plevel(k)-10.0
              else if( k ==  39 ) then
                plve1=plevel(k)+10.0
                plve2=plevel(k)-10.0
              else if( k ==  40 ) then
                plve1=plevel(k)+10.0
                plve2=plevel(k)-5.0
              else if( k ==41) then
                plve1=plevel(k)+5.0
                plve2=plevel(k)-5.0
              else if( k ==42) then
                plve1=plevel(k)+5.0
                plve2=plevel(k)-2.5
              else if ( k ==43) then
                plve1=plevel(k)+2.5
                plve2=0.0
              endif
              if(obs(1,kk) >plve2 .and. obs(1,kk) <=plve1) then
                 nkk=k
                 exit
              endif
            enddo 
!              print *, nkk
              if(nkk == 0) then
                print *,'nkk value is not valid'
                stop
              endif
!              print *,'wind ',ndata,hdr(7),nkk,obs(1,kk),obs(2,kk),ans(1,kk),bac(1,kk)
               if(obserr(kk) <1000.0) then
                  if(qms(kk) <0.0 .or. qms(kk) ==10.0 ) then 
                     w220_no(ndata,nkk,2)=w220_no(ndata,nkk,2)+1.0
                  else if(abs(obs(2,kk)) <2000.0 .and. abs(bac(1,kk)) <2000.0 .and. abs(ans(1,kk)) <2000.0) then 
                     obu=obs(2,kk)
                     obv=obs(3,kk)
                     bacu=bac(1,kk)
                     bacv=bac(2,kk)
                     ansu=ans(1,kk)
                     ansv=ans(2,kk)
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
                     if(obserr(kk) >100.0 ) obserr(kk)=3.5 
                     diff=(obs(2,kk)-bac(1,kk))**2+(obs(3,kk)-bac(2,kk))**2
                     diff=sqrt(diff)
                     ratio=diff/obserr(kk)
                     if(ratio >gross) then
                        w220_no(ndata,nkk,2)=w220_no(ndata,nkk,2)+1.0
                     else
                        w220_no(ndata,nkk,1)=w220_no(ndata,nkk,1)+1.0
! for u and v
                        w220(ndata,nkk,1)=w220(ndata,nkk,1)+obs(2,kk)-bac(1,kk)   ! obs-back(u)
                        w220(ndata,nkk,2)=w220(ndata,nkk,2)+obs(2,kk)-ans(1,kk)   ! obs-anl(u)
                        w220(ndata,nkk,3)=w220(ndata,nkk,3)+vqc(1,kk)             ! variational qc weight(bac)
                        w220(ndata,nkk,4)=w220(ndata,nkk,4)+vqc(2,kk)             ! variational qc weight(bac)
                        w220(ndata,nkk,5)=w220(ndata,nkk,5)+obs(3,kk)-bac(2,kk)   ! obs-back(v)
                        w220(ndata,nkk,6)=w220(ndata,nkk,6)+obs(3,kk)-ans(2,kk)   ! obs-anl(v)
                        w220(ndata,nkk,7)=w220(ndata,nkk,7)+obs(2,kk)             ! obs u
                        w220(ndata,nkk,8)=w220(ndata,nkk,8)+obs(3,kk)             ! obs v
!  for wind speed and direction
                        w220(ndata,nkk,9)=w220(ndata,nkk,9)+dif
                        w220(ndata,nkk,10)=w220(ndata,nkk,10)+difa
                        w220(ndata,nkk,11)=w220(ndata,nkk,11)+obv-bacv  !o-b speed
                        w220(ndata,nkk,12)=w220(ndata,nkk,12)+obv-ansv  !o-a speed
                        w220(ndata,nkk,13)=w220(ndata,nkk,13)+obu      ! obs direction
                        w220(ndata,nkk,14)=w220(ndata,nkk,14)+obv      ! obs speed
                     endif 
                  endif
               else if (obserr(kk) >1000.00 ) then
                  if(qms(kk) <0.0  .or. qms(kk) ==10.0) then
                     w220_no(ndata,nkk,2)=w220_no(ndata,nkk,2)+1.0
                  else if(abs(obs(2,kk)) <2000.0 .and. abs(bac(1,kk)) <2000.0 .and. abs(ans(1,kk)) <2000.0) then
                     diff=(obs(2,kk)-bac(1,kk))**2+(obs(3,kk)-bac(2,kk))**2
                     diff=sqrt(diff)
                     ratio=diff/3.0
                     if(ratio >13.0) then
                        w220_no(ndata,nkk,2)=w220_no(ndata,nkk,2)+1.0
                     else
                        w220_no(ndata,nkk,1)=w220_no(ndata,nkk,1)+1.0
! for u and v
                        w220(ndata,nkk,1)=w220(ndata,nkk,1)+obs(2,kk)-bac(1,kk)   ! obs-back(u)
                        w220(ndata,nkk,2)=w220(ndata,nkk,2)+obs(2,kk)-ans(1,kk)   ! obs-anl(u)
                        w220(ndata,nkk,3)=w220(ndata,nkk,3)+vqc(1,kk)             ! variational qc weight(bac)
                        w220(ndata,nkk,4)=w220(ndata,nkk,4)+vqc(2,kk)             ! variational qc weight(bac)
                        w220(ndata,nkk,5)=w220(ndata,nkk,5)+obs(3,kk)-bac(2,kk)   ! obs-back(v)
                        w220(ndata,nkk,6)=w220(ndata,nkk,6)+obs(3,kk)-ans(2,kk)   ! obs-anl(v)
                        w220(ndata,nkk,7)=w220(ndata,nkk,7)+obs(2,kk)             ! obs u
                        w220(ndata,nkk,8)=w220(ndata,nkk,8)+obs(3,kk)             ! obs v
!  for wind speed and direction
                        w220(ndata,nkk,9)=w220(ndata,nkk,9)+dif
                        w220(ndata,nkk,10)=w220(ndata,nkk,10)+difa
                        w220(ndata,nkk,11)=w220(ndata,nkk,11)+obv-bacv  !o-b speed
                        w220(ndata,nkk,12)=w220(ndata,nkk,12)+obv-ansv  !o-a speed
                        w220(ndata,nkk,13)=w220(ndata,nkk,13)+obu      ! obs direction
                        w220(ndata,nkk,14)=w220(ndata,nkk,14)+obv      ! obs speed
                     endif
                  endif
               endif
         enddo       !! end do nlev
       endif      !!  for type screen

     enddo      !! enddo ireadsub
   enddo      !! enddo  ireadmg 

           print *,'read_prepbufr_wind ',dtype,nread,ndata

!           write(6,100) (stdid(i),i=1,ndata)
!100 format(8a8)

               do i=1,ndata
               do k=1,43
                if(w220_no(i,k,1) >=1.0) then
                  do j=1,14
                    w220(i,k,j)=w220(i,k,j)/w220_no(i,k,1)
                  enddo
                else if(w220_no(i,k,1) ==0.0) then
                   do j=1,14
                    w220(i,k,j)=-999.0
                  enddo
                endif
             enddo
             enddo
           write(6,*) 'wind'
           write(6,200) (w220_no(1,k,1),k=1,43) 
           write(6,200) (w220_no(1,k,2),k=1,43) 
           write(6,200) (w220(1,k,1),k=1,43) 
           write(6,200) (w220_no(5,k,1),k=1,43) 
           write(6,200) (w220_no(5,k,2),k=1,43) 
           write(6,200) (w220(5,k,1),k=1,43) 
200 format(10f7.2)


       if(ndata >0) then 

          dtype=trim(dtype)
         open(40,file=dtype,form='unformatted')
           write(40) ndata
           write(40) stdid(1:ndata),cid(1:ndata,1:6),w220_no(1:ndata,1:43,1:2),w220(1:ndata,1:43,1:14)
           close(40)
        endif 


          close(11)
          return 
          end     
   

 
