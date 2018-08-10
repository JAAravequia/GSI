!=====================================
! read_gps
!
!   read gps data files
!=====================================

subroutine read_gps(nreal,dtype,fname,fileo,gtross,rlev)

   implicit none

   real(4),allocatable,dimension(:,:)  :: rdiag
   real(4),dimension(3,3000000) :: rpress
   integer,dimension(3) :: ncount,ncount_vqc,ncount_gros

   character*200 fname
   character*50 fileo
   character*15 dtype 

   real*4 tiny,huge,real
   real rlev,rgtross,gtross,weight,ddf
   real max_bend, min_bend

   integer nobs,nreal,ntotal,ngross,nreal_in,nlev
   integer ilat,ilon,ipres,itime,iqc,iuse,imuse,iweight,ierr,ierr2,ierr3,iobs,iogs,iqsges
   integer iheight, ispread, ihob, ibend, izsges, iibend, iq, itemp
   integer i,ndup,ioges,igos

   integer,dimension(5) :: iqc_flag

   real(4) :: rmiss,vqclmt,vqclmte

   data rmiss / -999.0 / 
   data huge / 1.0e6 /
   data tiny / 1.0e-6 /

   max_bend = tiny
   min_bend = huge

   ncount=0
   rpress=rmiss
   ncount_vqc=0
   ncount_gros=0
   iqc_flag=0

   print *, '--> read_gps'
   print *, '      nreal, dtype = ', nreal, dtype
   print *, '      fname, fileo = ', fname, fileo
   print *, '      gtross, rlev = ', gtross, rlev

   ntotal=0
   open(unit=11,file=fname,form='unformatted')
   rewind(11)

   read(11) nobs,nreal_in
   print *, 'nobs=',nobs

   if (nreal /= nreal_in) then
      print *,'nreal_in,nreal ',nreal_in,nreal
      stop
   endif 

   allocate(rdiag(nreal,nobs))
   read(11) rdiag


!!!!-----------------------------------------------
!!!  from setupbend.f90
!!
!!   rdiagbuf(1,i)         = ictype(ikx)        ! observation type
!!   rdiagbuf(2,i)         = data(iprof,i)      ! profile identifier
!!   rdiagbuf(3,i)         = data(ilate,i)      ! lat in degrees
!!   rdiagbuf(4,i)         = data(ilone,i)      ! lon in degrees
!!   rdiagbuf(5,i)         = (data(igps,i)-dbend)/data(igps,i) ! incremental bending angle (x100 %)
!!   rdiagbuf(6,i)         = ten*exp(dpressure) ! pressure at obs location
!!   rdiagbuf(7,i)         = tpdpres(i)-rocprof ! impact height in meters
!!   rdiagbuf(8,i)         = dtime-time_offset  ! obs time (hours relative to analysis time)
!!   rdiagbuf(9,i)         = zsges              ! model terrain (m)
!!   rdiagbuf(10,i)        = one|two|three|four ! qc failure flags, 0 = good
!!   rdiagbuf(11,i)        = data(iuse,i)       ! data usage flag
!!   rdiagbuf(12,i)        = one|-one           ! muse flag, 1=use, -1=not used
!!   rdiagbuf(13,i)        = zero               ! nonlinear qc relative weight - will be defined in genstats_gps
!!   rdiagbuf(14,i)        = errinv_input       ! original inverse gps obs error (rad**-1)
!!   rdiagbuf(15,i)        = errinv_adjst       ! original + represent error inverse gps obs error (rad**-1)
!!   rdiagbuf(16,i)        = errinv_final       ! final inverse observation error due to
!!                                              ! superob factor (rad**-1) and qc modified in genstats_gps
!!   rdiagbuf(17,i)        = data(igps,i)       ! bending angle observation (radians)
!!
!!   rdiagbuf(18,i)        = trefges            ! temp at obs location (Kelvin) !if monotone grid
!!   rdiagbuf(19,i)        = hob                ! model vertical grid (interface) if monotone grid
!!   rdiagbuf(20,i)        = one                ! uses gps_ref (one = use of bending angle)
!!   rdiagbuf(21,i)        = qrefges            ! spedific humidity at obs !location (kg/kg) if monotone grid
!!   rdiagbuf(22,i)        = 1.e+10_r_kind      ! spread (filled in by EnKF)
!
!  Note that when stored in the data file the first 2 fields
!  in rdiagbuf have been stripped so the values will be -2
!  from the above values.
!!---------------------------------------------------------

   ilat=1                          !  lat (degrees)
   ilon=2                          !  lon (degrees)
   iibend=3                        !  incremental bending angle
   ipres=4                         !  pressure
   iheight=5                       !  impact height in meters
   itime=6                         !  relative time
   izsges=7                        !  model terrain (m)
   iqc=8                           !  qc flag, 0=good
   iuse=9                          !  original data usage flag
   imuse=10                        !  data usage flag in the analsis
   iweight=11                      !  variational relative weight
   ierr=12                         !  original error from bufr error table
   ierr2=13                        !  error from read_bufr
   ierr3=14                        !  error from final adjusted
   ibend=15                        !  bending angle observation (radians)
   itemp=16                        !  temp at obs location (Kelvin) if monotone grid
   ihob=17                         !  model vertical grid (interface) if monotone grid
   iq=19                           !  specific humidity at obs location (kg/kg) if monotone grid
   ispread=20                      !  spread (filled in by EnKF)


   print *, 'field 1, lat    = ', rdiag(ilat,1)
   print *, 'field 2, lon    = ', rdiag(ilon,1)
   print *, 'field 3, ibend  = ', rdiag(iibend,1)
   print *, 'field 4, press  = ', rdiag(ipres,1)
   print *, 'field 5, height = ', rdiag(iheight,1)
   print *, 'field 6, time   = ', rdiag(itime,1)
   print *, 'field 7, terrain = ', rdiag(izsges,1)
   print *, 'field 8, qc     = ', rdiag(iqc,1)
   print *, 'field 9, use    = ', rdiag(iuse,1)
   print *, 'field 10, muse  = ', rdiag(imuse,1)
   print *, 'field 11, weight = ', rdiag(iweight,1)
   print *, 'field 12, ierr  = ', rdiag(ierr,1)
   print *, 'field 13, ierr2 = ', rdiag(ierr2,1)
   print *, 'field 14, ierr3 = ', rdiag(ierr3,1)
   print *, 'field 15, bend  = ', rdiag(ibend,1)
   print *, 'field 16, temp  = ', rdiag(itemp,1)
   print *, 'field 17, hob   = ', rdiag(ihob,1)
   print *, 'field 18,       = ', rdiag(18,1)
   print *, 'field 19, q     = ', rdiag(iq,1)
   print *, 'field 20, spread = ', rdiag(ispread,1)


   call rm_dups( rdiag,nobs,nreal,ilat,ilon,ipres,itime,iweight,ndup )

   do  i=1,nobs

      if( rdiag(iuse,i) < 0.0 .or. rdiag(iuse,i) > 0.0 ) then
         print *, 'NON-ZERO iuse value:  rdiag(iuse,i) = ', rdiag(iuse,i)
      end if


      if( rdiag(iqc,i) <= 0.0 ) then

         print *, 'press = ', rdiag(ipres,i)

         if( rdiag(iibend,i) < min_bend ) then
            min_bend = rdiag(iibend,i)
         end if

         if( rdiag(iibend,i) > max_bend ) then
            max_bend = rdiag(iibend,i)
         end if
        

         iqc_flag(1)=iqc_flag(1) + 1
!         if( rdiag(iweight,i> >= 0.0 .and. rdiag(imuse,i) >0.0 ) then
!            ncount(1)=ncount(1)+1
!         fi 
      else if( rdiag(iqc,i) <=1.0 ) then 
         iqc_flag(2)=iqc_flag(2) + 1
      else if( rdiag(iqc,i) <=2.0 ) then 
         iqc_flag(3)=iqc_flag(3) + 1
      else if( rdiag(iqc,i) <=3.0 ) then 
         iqc_flag(4)=iqc_flag(4) + 1
      else if( rdiag(iqc,i) <=4.0 ) then 
         iqc_flag(5)=iqc_flag(5) + 1
      else
         print *, 'rdiag(iqc,i) is > 4.0 :: ', rdiag(iqc,i)
      end if

      if( rdiag(imuse,i) >0.0 .and. rdiag(iweight,i) >= 0.0 ) then      ! assim data
         ncount(1)=ncount(1)+1
         rpress(1,ncount(1))=rdiag(iibend,i)*50
!         rpress(1,ncount(1))=rdiag(iogs,i)*rdiag(ierr,i)

         if( rdiag(iqc,i) > 0.0 .and. rdiag(iqc,i) <= 2.0 ) then
            ncount_vqc(1) = ncount_vqc(1) + 1
         else if( rdiag(iqc,i) > 0.0 .and. rdiag(iqc,i) <= 3.0 ) then
            ncount_gros(1) = ncount_gros(1) + 1
         else if( rdiag(iqc,i) > 3.0 ) then                             ! vqc rejected data
            ncount_vqc(1)=ncount_vqc(1)+1 
         end if

      else if( rdiag(imuse,1) < 0.0 ) then                              ! monitored data
         ncount(3)=ncount(3)+1
         rpress(3,ncount(3))=rdiag(iibend,i)*50

         if( rdiag(iqc,i) > 0.0 .and. rdiag(iqc,i) <= 2.0 ) then
            ncount_vqc(3) = ncount_vqc(3) + 1
         else if( rdiag(iqc,i) > 2.0 .and. rdiag(iqc,i) <= 3.0 ) then    
            ncount_gros(3) = ncount_gros(3) + 1
         else if( rdiag(iqc,i) > 3.0 .and. rdiag(iqc,i) <= 4.0 ) then          
            ncount_vqc(3) = ncount_vqc(3) + 1
         end if

      end if

!      if( rdiag(iweight,i) >= 0.0 .and. rdiag(imuse,i) >0.0 ) then
!         ncount(1)=ncount(1)+1
!         rpress(1,ncount(1))=rdiag(iogs,i)*rdiag(ierr,i)
!         print *, 'rpress(1,ncount(1)) = ', rpress(1,ncount(1))
!         weight=rdiag(iweight,i)

!         if(weight <1.0) then
!            ncount_vqc(1)=ncount_vqc(1)+1 
!            vqclmt=vqclmt+abs(rdiag(ioges,i))*rdiag(ierr,i)
!         endif

!         if(rdiag(iqc,i) >=4.0) then
!            ncount(2)=ncount(2)+1
!           if(weight <1.0) then
!              ncount_vqc(2)=ncount_vqc(2)+1 
!           endif
!            rpress(2,ncount(2))=rdiag(iogs,i)*rdiag(ierr,i)
!         endif

!      else if(rdiag(iweight,i) >= 0.0 .and. rdiag(imuse,i) <0.0) then
!         print *, 'rdiag(iweight,i), rdiag(imuse,i), rdiag(iqc,i) = ', rdiag(iweight,i), rdiag(imuse,i), rdiag(iqc,i)
!         if(rdiag(iqc,i) <=7.0) then
!           ncount_gros(1)=ncount_gros(1)+1
!           if(rdiag(iqc,i) >3.0 .and. rdiag(iqc,i) <=7.0) then
!              ncount_gros(2)=ncount_gros(2)+1
!           endif

!         else if(rdiag(iqc,i) >=8.0 ) then  

!            if(rdiag(ierr,i) >tiny ) then
!               ddf=abs(rdiag(igos,i))*rdiag(ierr,i) 
!               if(ddf <gtross) then
!                 ncount(3)=ncount(3)+1
!                  rpress(3,ncount(3))=rdiag(iogs,i)*rdiag(ierr,i)
!                  print *, 'rpress(3,ncount(3)) = ', rpress(3,ncount(3))
!              else
!                 ncount_gros(3)=ncount_gros(3)+1
!               endif
!            else
!              ncount(3)=ncount(3)+1
!               rpress(3,ncount(3))=rdiag(iogs,i)/rdiag(iqsges,i)
!               print *, 'rpress(3,ncount(3)) = ', rpress(3,ncount(3))
!            endif
!         endif

!      endif
   enddo

   print *, 'ncount(1) = ', ncount(1)           
   print *, 'ncount(2) = ', ncount(2)           
   print *, 'ncount(3) = ', ncount(3)           
   print *, 'ncount_vqc(1) = ', ncount_vqc(1)           
   print *, 'ncount_vqc(2) = ', ncount_vqc(2)           
   print *, 'ncount_vqc(3) = ', ncount_vqc(3)           
   print *, 'ncount_gros(1) = ', ncount_gros(1)           
   print *, 'ncount_gros(2) = ', ncount_gros(2)           
   print *, 'ncount_gros(3) = ', ncount_gros(3)           

   print *, '0, iqc_flag(1) = ', iqc_flag(1)           
   print *, '1, iqc_flag(2) = ', iqc_flag(2)           
   print *, '2, iqc_flag(3) = ', iqc_flag(3)           
   print *, '3, iqc_flag(4) = ', iqc_flag(4)           
   print *, '4, iqc_flag(5) = ', iqc_flag(5)           

   deallocate(rdiag)


!   if(ncount_vqc(1) >0) then
!      vqclmt=vqclmt/ncount_vqc(1)
!   endif

!  calculate the the areacd usr2
   rgtross=-gtross
   nlev=nint((gtross-rgtross)/rlev) 

   print *, 'min_bend, max_bend = ', min_bend, max_bend
    
   print *,nlev
   print *, 'rmax,rmin ',gtross,rgtross
   print *, 'ncount_gros,',ncount_gros(1),ncount_gros(2),ncount_gros(3) 
   print *, 'vqc-limit,vqc-limite ',vqclmt,vqclmte
    
    
   call hist(dtype,rpress,3,3000000,ncount,rgtross,gtross,rlev,fileo,ncount_vqc,ncount_gros)   
!   print *, '<-- read_gps'
   return 
end
