module readsatobs
!$$$  module documentation block
!
! module: readsatobs                   read data from satellite radiance
!                                      diag* files.
!
! prgmmr: whitaker         org: esrl/psd               date: 2009-02-23
!
! abstract: read data from satellite radiance diag* files written out
!  by GSI forward operator code.
!
! Public Subroutines:
!  get_num_satobs: determine the number of observations to read.
!  get_satobs_data: read the data.
!   
! Public Variables: 
!
! Modules Used: read_diag
!
! program history log:
!   2009-02-23  Initial version.
!   2016-06-03  Collard - Added changes to allow for historical naming conventions
!
! attributes:
!   language: f95
!
!$$$
  
use kinds, only: r_kind,i_kind,r_single
use read_diag, only: diag_data_fix_list,diag_header_fix_list,diag_header_chan_list, &
    diag_data_chan_list,diag_data_extra_list,read_radiag_data,read_radiag_header, &
    diag_data_name_list
use params, only: nsats_rad, nsatmax_rad, dsis, sattypes_rad, npefiles

implicit none

private
public :: get_satobs_data, get_num_satobs, write_satobs_data

contains

subroutine get_num_satobs(obspath,datestring,num_obs_tot,num_obs_totdiag,id)
    use radinfo, only: iuse_rad,nusis,jpch_rad,npred
    character (len=500), intent(in) :: obspath
    character(len=500) obsfile
    character(len=10), intent(in) :: id, datestring
    character(len=20) ::  sat_type
    character(len=4) :: pe_name
    integer(i_kind), intent(out) :: num_obs_tot, num_obs_totdiag
    integer(i_kind) iunit, iflag, nsat, ios,n,nkeep, i, jpchstart,indxsat,ipe
    integer(i_kind) npred_radiag
    logical fexist,lretrieval,lverbose,init_pass
    real(r_kind) :: errorlimit,errorlimit2

    type(diag_header_fix_list )         :: header_fix0
    type(diag_header_chan_list),allocatable :: header_chan0(:)
    type(diag_data_fix_list   )         :: data_fix0
    type(diag_data_chan_list  ),allocatable :: data_chan0(:)
    type(diag_data_extra_list) ,allocatable :: data_extra0(:,:)
    type(diag_data_name_list)           :: data_name0

!  make consistent with screenobs
    errorlimit=1._r_kind/sqrt(1.e9_r_kind)
    errorlimit2=1._r_kind/sqrt(1.e-6_r_kind)
    iunit = 7
    lretrieval=.false.
    npred_radiag=npred
    lverbose=.false.

    num_obs_tot = 0
    num_obs_totdiag = 0
    do nsat=1,nsats_rad
        jpchstart=0
        do i=1,jpch_rad
          write(sat_type,'(a20)') adjustl(dsis(nsat))
          ! The following is to sort out some historical naming conventions
          select case (sat_type(1:4))
             case ('airs')
               sat_type='airs_aqua'
             case ('iasi')
               if (index(sat_type,'metop-a') /= 0) sat_type='iasi_metop-a'
               if (index(sat_type,'metop-b') /= 0) sat_type='iasi_metop-b'
               if (index(sat_type,'metop-c') /= 0) sat_type='iasi_metop-c'
          end select
    
          if(sat_type == trim(nusis(i)) .and. iuse_rad(i) > 0) then
            jpchstart=i
            exit
          end if
        end do
        if(jpchstart == 0) cycle
        init_pass = .true.
        peloop: do ipe=0,npefiles
           write(pe_name,'(i4.4)') ipe
           if (npefiles .eq. 0) then
               ! read diag file (concatenated pe* files)
               obsfile = trim(adjustl(obspath))//"diag_"//trim(sattypes_rad(nsat))//"_ges."//datestring//'_'//trim(adjustl(id))
               inquire(file=obsfile,exist=fexist)
               if (.not. fexist .or. datestring .eq. '0000000000') &
               obsfile = trim(adjustl(obspath))//"diag_"//trim(sattypes_rad(nsat))//"_ges."//trim(adjustl(id))
           else ! read raw, unconcatenated pe* files.
               obsfile =&
               trim(adjustl(obspath))//'gsitmp_'//trim(adjustl(id))//'/pe'//pe_name//'.'//trim(sattypes_rad(nsat))//'_01'
           endif

           inquire(file=obsfile,exist=fexist)
           if (.not.fexist) cycle peloop
           nkeep = 0

           !print *,'obsfile=',trim(obsfile)

           open(iunit,form="unformatted",file=obsfile,iostat=ios)
           rewind(iunit)
           if (init_pass) then
              call read_radiag_header(iunit,npred_radiag,lretrieval,header_fix0,header_chan0,data_name0,iflag,lverbose)
              init_pass = .false.
           endif

           do
              call read_radiag_data(iunit,header_fix0,lretrieval,data_fix0,data_chan0,data_extra0,iflag )
              if( iflag /= 0 )exit
              chan: do n=1,header_fix0%nchan
                num_obs_totdiag = num_obs_totdiag + 1
                if(header_chan0(n)%iuse<1) cycle chan
                indxsat=header_chan0(n)%iochan
                if(data_chan0(n)%qcmark < 0. .or. data_chan0(n)%errinv < errorlimit &
                         .or. data_chan0(n)%errinv > errorlimit2 &
                         .or. indxsat == 0) cycle chan
                if (header_fix0%iextra > 0) then
                   if(data_extra0(1,n)%extra <= 0.001_r_kind .or.  &
                      data_extra0(1,n)%extra > 1200._r_kind  .or. &
                      abs(data_chan0(n)%tbobs) > 1.e9_r_kind) cycle chan
                else
                   if(abs(data_chan0(n)%tbobs) > 1.e9_r_kind) cycle chan
                endif
                nkeep = nkeep + 1
              end do chan
           enddo
           num_obs_tot = num_obs_tot + nkeep
           close(iunit)
           if (ipe .eq. npefiles) then
              write(6,100) nsat,trim(sattypes_rad(nsat)),num_obs_tot
100           format(2x,i3,2x,a20,2x,'num_obs_tot= ',i9)
           endif
        enddo peloop ! ipe
    enddo ! satellite
end subroutine get_num_satobs

subroutine get_satobs_data(obspath, datestring, nobs_max, nobs_maxdiag, h_x, h_xnobc, x_obs, x_err, &
           x_lon, x_lat, x_press, x_time, x_channum, x_errorig, x_type, x_biaspred, x_indx, x_used, id,id2)
  use radinfo, only: iuse_rad,nusis,jpch_rad,npred,adp_anglebc,emiss_bc
  character*500, intent(in) :: obspath
  character*500 obsfile,obsfile2
  character(len=10), intent(in) :: id,id2
  character(len=4) pe_name

  real(r_single), dimension(nobs_max) :: h_x,h_xnobc,x_obs,x_err,x_lon,&
                               x_lat,x_press,x_time,x_errorig
  real(r_single), dimension(npred+1,nobs_max) :: x_biaspred
  integer(i_kind), dimension(nobs_max) ::  x_channum,x_indx
  integer(i_kind), dimension(nobs_maxdiag) :: x_used
  character(len=20), dimension(nobs_max) ::  x_type
  character(len=20) ::  sat_type
  character(len=10), intent(in) ::  datestring

  integer(i_kind) nobs_max, nobs_maxdiag, iunit, iunit2,iflag,nobs,nobsdiag, n,nsat,ipe,i,jpchstart,indxsat
  integer(i_kind) npred_radiag,iflag2
  logical twofiles,fexist1,fexist2,lretrieval,lverbose,init_pass,init_pass2
  real(r_kind) :: errorlimit,errorlimit2

  type(diag_header_fix_list )         :: header_fix1,header_fix2
  type(diag_header_chan_list),allocatable :: header_chan1(:),header_chan2(:)
  type(diag_data_fix_list   )         :: data_fix1,data_fix2
  type(diag_data_chan_list  ),allocatable :: data_chan1(:),data_chan2(:)
  type(diag_data_extra_list) ,allocatable :: data_extra1(:,:),data_extra2(:,:)
  type(diag_data_name_list)           :: data_name1,data_name2

! make consistent with screenobs
  errorlimit=1._r_kind/sqrt(1.e9_r_kind)
  errorlimit2=1._r_kind/sqrt(1.e-6_r_kind)

  iunit = 7
  iunit2 = 17
  lretrieval=.false.
  npred_radiag=npred
  lverbose=.false.

  nobs = 0
  nobsdiag = 0
  x_used = 0
  twofiles = id /= id2

  do nsat=1,nsats_rad
     jpchstart=0
     do i=1,jpch_rad

       write(sat_type,'(a20)') adjustl(dsis(nsat))
       ! The following is to sort out some historical naming conventions
       select case (sat_type(1:4))
          case ('airs')
            sat_type='airs_aqua'
          case ('iasi')
            if (index(sat_type,'metop-a') /= 0) sat_type='iasi_metop-a'
            if (index(sat_type,'metop-b') /= 0) sat_type='iasi_metop-b'
            if (index(sat_type,'metop-c') /= 0) sat_type='iasi_metop-c'
       end select
    
      if(sat_type == trim(nusis(i)) .and. iuse_rad(i) > 0) then
         jpchstart = i
         exit
       end if
     end do
     if(jpchstart == 0) cycle
     init_pass = .true.; init_pass2 = .true.
     peloop: do ipe=0,npefiles
     write(pe_name,'(i4.4)') ipe
     if (npefiles .eq. 0) then
         ! read diag file (concatenated pe* files)
         obsfile = trim(adjustl(obspath))//"diag_"//trim(sattypes_rad(nsat))//"_ges."//datestring//'_'//trim(adjustl(id))
         inquire(file=obsfile,exist=fexist1)
         if (.not. fexist1 .or. datestring .eq. '0000000000') &
         obsfile = trim(adjustl(obspath))//"diag_"//trim(sattypes_rad(nsat))//"_ges."//trim(adjustl(id))
     else ! read raw, unconcatenated pe* files.
         obsfile =&
         trim(adjustl(obspath))//'gsitmp_'//trim(adjustl(id))//'/pe'//pe_name//'.'//trim(sattypes_rad(nsat))//'_01'
     endif
     inquire(file=obsfile,exist=fexist1)
     if(.not.fexist1) cycle peloop

     open(iunit,form="unformatted",file=obsfile)
     rewind(iunit)
     if (init_pass) then
        call read_radiag_header(iunit,npred_radiag,lretrieval,header_fix1,header_chan1,data_name1,iflag,lverbose)
        init_pass = .false.
     endif

     if(twofiles)then
        if (npefiles .eq. 0)  then
          ! read diag file (concatenated pe* files)
          obsfile2 = trim(adjustl(obspath))//"diag_"//trim(sattypes_rad(nsat))//"_ges."//datestring//'_'//trim(adjustl(id2))
          inquire(file=obsfile2,exist=fexist2)
          if (.not. fexist2 .or. datestring .eq. '0000000000') &
          obsfile2 = trim(adjustl(obspath))//"diag_"//trim(sattypes_rad(nsat))//"_ges."//trim(adjustl(id2))
       else ! read raw, unconcatenated pe* files.
          obsfile2 =&
          trim(adjustl(obspath))//'gsitmp_'//trim(adjustl(id2))//'/pe'//pe_name//'.'//trim(sattypes_rad(nsat))//'_01'
       endif

       open(iunit2,form="unformatted",file=obsfile2)
       rewind(iunit2)
       if (init_pass2) then
          call read_radiag_header(iunit2,npred_radiag,lretrieval,header_fix2,header_chan2,data_name2,iflag2,lverbose)
          init_pass2 = .false.
       endif
     end if

     do
      call read_radiag_data(iunit,header_fix1,lretrieval,data_fix1,data_chan1,data_extra1,iflag )
      if( iflag /= 0 ) then
       exit
      end if
      if(twofiles)then
         call read_radiag_data(iunit2,header_fix2,lretrieval,data_fix2,data_chan2,data_extra2,iflag2 )
        if( header_fix1%nchan /= header_fix2%nchan .or. abs(data_fix1%lat-data_fix2%lat) .gt. 1.e-5 .or.  &
            abs(data_fix1%lon-data_fix2%lon) .gt. 1.e-5 .or. abs(data_fix1%obstime-data_fix2%obstime) .gt. 1.e-5) then
           write(6,*) 'inconsistent files',trim(obsfile2)
           write(6,*) 'nchan',header_fix1%nchan,header_fix2%nchan
           write(6,*) 'lat',data_fix1%lat,data_fix2%lat
           write(6,*) 'lon',data_fix1%lon,data_fix2%lon
           write(6,*) 'obstim',data_fix1%obstime,data_fix2%obstime
           call stop2(-99)
        end if
      end if
      chan:do n=1,header_fix1%nchan
         nobsdiag = nobsdiag + 1
         if(header_chan1(n)%iuse<1) cycle chan
         indxsat=header_chan1(n)%iochan
         if(data_chan1(n)%qcmark < 0. .or. data_chan1(n)%errinv < errorlimit &
                  .or. data_chan1(n)%errinv > errorlimit2 &
                  .or. indxsat == 0) cycle chan
         if (header_fix1%iextra > 0) then
            if(data_extra1(1,n)%extra <= 0.001_r_kind .or.  &
               data_extra1(1,n)%extra > 1200._r_kind  .or.  &
               abs(data_chan1(n)%tbobs) > 1.e9_r_kind) cycle chan
         else
            if(abs(data_chan1(n)%tbobs) > 1.e9_r_kind) cycle chan
         endif
         nobs = nobs + 1 
         x_used(nobsdiag) = 1
         if (nobs > nobs_max) then
             print *,'warning:  exceeding array bounds in readinfo_from_file',&
             nobs,nobs_max
         end if
         x_type(nobs)= sat_type
         x_channum(nobs) = n
         x_indx(nobs) = indxsat
         x_lon(nobs) = data_fix1%lon
         x_lat(nobs) = data_fix1%lat
         x_time(nobs) = data_fix1%obstime
         x_obs(nobs) = data_chan1(n)%tbobs 
         ! bias corrected Hx
         h_x(nobs) = x_obs(nobs) - data_chan1(n)%omgbc 
         ! un-bias corrected Hx
         if(twofiles)then
           h_xnobc(nobs) = x_obs(nobs) - data_chan2(n)%omgnbc
         else
           h_xnobc(nobs) = x_obs(nobs) - data_chan1(n)%omgnbc
         end if
         ! data_chan%errinv is inverse error variance.
         x_errorig(nobs) = header_chan1(n)%varch**2
         x_err(nobs) = (1._r_kind/data_chan1(n)%errinv)**2
         if (header_fix1%iextra > 0) then
           x_press(nobs) = data_extra1(1,n)%extra
         else
           x_press(nobs) = 99999
         endif

!! DTK:  **NOTE**
!!       The bifix term will need to be expanded if/when the GSI/GDAS goes to using
!!       a higher polynomial version of the angle dependent bias correction (if
!!       and when it is moved into part of the varbc)
!!         x_biaspred(1,nobs) = data_chan1(n)%bifix! fixed angle dependent bias
         x_biaspred(1,nobs) = data_chan1(n)%bifix(1) ! fixed angle dependent bias
         x_biaspred(2,nobs) = data_chan1(n)%bicons ! constant bias correction
         x_biaspred(3,nobs) = data_chan1(n)%biang ! scan angle bias correction
         x_biaspred(4,nobs) = data_chan1(n)%biclw ! CLW bias correction
         x_biaspred(5,nobs) = data_chan1(n)%bilap2 ! square lapse rate bias corr
         x_biaspred(6,nobs) = data_chan1(n)%bilap ! lapse rate bias correction
         if (npred == 7) then
           x_biaspred(7,nobs) = data_chan1(n)%bicos ! node*cos(lat) bias correction for SSMIS
           x_biaspred(8,nobs) = data_chan1(n)%bisin ! sin(lat) bias correction for SSMIS                    
         endif
         if (emiss_bc) x_biaspred(9,nobs) = data_chan1(n)%biemis

         if (adp_anglebc) then
            x_biaspred( 1,nobs)  = data_chan1(n)%bifix(5) ! fixed angle dependent bias correction
            x_biaspred(npred-2,nobs)  = data_chan1(n)%bifix(1) ! 4th order scan angle (predictor)
            x_biaspred(npred-1,nobs)  = data_chan1(n)%bifix(2) ! 3rd order scan angle (predictor)
            x_biaspred(npred,nobs)  = data_chan1(n)%bifix(3) ! 2nd order scan angle (predictor)
            x_biaspred(npred+1,nobs)    = data_chan1(n)%bifix(4) ! 1st order scan angle (predictor)
         endif

      enddo chan
     enddo

     cycle

900  continue
     close(iunit)
     if(twofiles)close(iunit2)
     enddo peloop ! ipe
 enddo ! satellite

  if (nobs /= nobs_max) then
      print *,'number of obs not what expected in get_satobs_data',nobs,nobs_max
      call stop2(92)
  end if
  if (nobsdiag /= nobs_maxdiag) then
      print *,'number of total diag obs not what expected in get_satobs_data',nobsdiag,nobs_maxdiag
      call stop2(92)
  end if

 end subroutine get_satobs_data

subroutine write_satobs_data(obspath, datestring, nobs_max, nobs_maxdiag, x_fit, x_sprd, x_used, id, id2, gesid2)
  use radinfo, only: iuse_rad,jpch_rad,nusis
  use read_diag, only: iversion_radiag_2, ireal_radiag, ireal_old_radiag


  character*500, intent(in) :: obspath
  character*500 obsfile,obsfile2
  character(len=10), intent(in) :: id,id2,gesid2
  character(len=4) pe_name

  real(r_single), dimension(nobs_max) :: x_fit, x_sprd
  character(len=20) ::  sat_type
  character(len=10), intent(in) ::  datestring
  integer(i_kind), dimension(nobs_maxdiag) :: x_used

  integer(i_kind) nobs_max, nobs_maxdiag,iunit,iunit2,iflag,nobs, nobsdiag,n,nsat,ipe,i,jpchstart
  logical fexist,init_pass

  character(len=10):: satid,sentype
  character(len=20):: sensat

  integer(i_kind):: jiter,nchanl,npred,ianldate,ireal,ipchan,iextra,jextra
  integer(i_kind):: idiag,angord,iversion,inewpc,isens
  integer(i_kind):: iuse_tmp,nuchan_tmp,iochan_tmp
  real(r_single) :: freq_tmp,polar_tmp,wave_tmp,varch_tmp,tlapmean_tmp

  real(r_single),dimension(:,:),allocatable :: data_tmp
  real(r_single),dimension(:),allocatable   :: fix_tmp
  real(r_single),dimension(:,:),allocatable :: extra_tmp


  iunit = 7
  iunit2 = 17

  nobs = 0
  nobsdiag = 0

  do nsat=1,nsats_rad
     jpchstart=0
     do i=1,jpch_rad
       write(sat_type,'(a20)') adjustl(dsis(nsat))
          ! The following is to sort out some historical naming conventions
          select case (sat_type(1:4))
             case ('airs')
               sat_type='airs_aqua'
             case ('iasi')
               if (index(sat_type,'metop-a') /= 0) sat_type='iasi_metop-a'
               if (index(sat_type,'metop-b') /= 0) sat_type='iasi_metop-b'
               if (index(sat_type,'metop-c') /= 0) sat_type='iasi_metop-c'
          end select

       if(sat_type == trim(nusis(i)) .and. iuse_rad(i) > 0) then
         jpchstart = i
         exit
       end if
     end do
     if(jpchstart == 0) cycle
     init_pass = .true.
     obsfile2 = trim(adjustl(obspath))//"diag_"//trim(sattypes_rad(nsat))//"_"//trim(adjustl(gesid2))//"."//datestring//'_'//trim(adjustl(id2))
     peloop: do ipe=0,npefiles
     write(pe_name,'(i4.4)') ipe
     if (npefiles .eq. 0) then
        ! diag file (concatenated pe* files)
        obsfile = trim(adjustl(obspath))//"diag_"//trim(sattypes_rad(nsat))//"_ges."//datestring//'_'//trim(adjustl(id))
        inquire(file=obsfile,exist=fexist)
        if (.not. fexist .or. datestring .eq. '0000000000') then
           obsfile = trim(adjustl(obspath))//"diag_"//trim(sattypes_rad(nsat))//"_ges."//trim(adjustl(id))
        endif
     else ! raw, unconcatenated pe* files.
        obsfile = trim(adjustl(obspath))//'gsitmp_'//trim(adjustl(id))//'/pe'//pe_name//'.'//trim(sattypes_rad(nsat))//'_01'
     endif
     inquire(file=obsfile,exist=fexist)
     if(.not.fexist) cycle peloop

     open(iunit,form="unformatted",file=obsfile)
     rewind(iunit)
     if (init_pass) then
        open(iunit2,form="unformatted",file=obsfile2)
        ! Read header (fixed_part).
        read(iunit,IOSTAT=iflag) sensat,satid,sentype,jiter,nchanl,npred,ianldate,      &
               ireal,ipchan,iextra,jextra,idiag,angord,iversion,inewpc,isens
        if (iflag==0) then
           write(iunit2) sensat,satid,sentype,jiter,nchanl,npred,ianldate, &
                  ireal,ipchan,iextra,jextra,idiag,angord,iversion,inewpc,isens
        else
           rewind(iunit)
           read(iunit,IOSTAT=iflag) sensat,satid,sentype,jiter,nchanl,npred,ianldate,    &
                 ireal,ipchan,iextra,jextra,idiag,angord,iversion,inewpc
           if (iflag==0) then
              write(iunit2) sensat,satid,sentype,jiter,nchanl,npred,ianldate, &
                     ireal,ipchan,iextra,jextra,idiag,angord,iversion,inewpc
           else
              rewind(iunit)
              read(iunit,IOSTAT=iflag) sensat,satid,sentype,jiter,nchanl,npred,ianldate, &
                     ireal,ipchan,iextra,jextra
              if (iflag==0) then
                 write(iunit2) sensat,satid,sentype,jiter,nchanl,npred,ianldate, &
                       ireal,ipchan,iextra,jextra
              else
                 write(6,*)'READ_RADIAG_HEADER:  ***ERROR*** Unknown file format.Cannot read'
                 call stop2(5555)
              endif
           endif
        endif
        ! read header (channel part)
        do n=1, nchanl
           read(iunit,IOSTAT=iflag) freq_tmp,polar_tmp,wave_tmp,varch_tmp,tlapmean_tmp,  &
                   iuse_tmp,nuchan_tmp,iochan_tmp
           write(iunit2) freq_tmp,polar_tmp,wave_tmp,varch_tmp,tlapmean_tmp,iuse_tmp,    &
                   nuchan_tmp,iochan_tmp
           if (iflag/=0) return
        end do
        init_pass = .false.
     endif
     allocate(data_tmp(idiag,nchanl))
     if (iversion < iversion_radiag_2) then
        allocate( fix_tmp( ireal_old_radiag ) )
     else
        allocate( fix_tmp( ireal_radiag ) )
     end if
     if (iextra > 0) then
        allocate(extra_tmp(iextra,jextra))
     endif

     do
        if (iextra == 0) then
           read(iunit,IOSTAT=iflag) fix_tmp, data_tmp
        else
           read(iunit,IOSTAT=iflag) fix_tmp, data_tmp, extra_tmp
        endif
        if( iflag /= 0 ) then
           exit
        end if
        data_tmp(2,:) = -1.e10
        data_tmp(3,:) = 1.e10
        chan:do n=1,nchanl
           ! skip channel if it's not used
           nobsdiag = nobsdiag + 1
           if (x_used(nobsdiag) == 1) then
              nobs = nobs + 1
              data_tmp(2,n) = x_fit(nobs)
              data_tmp(3,n) = x_sprd(nobs)
           endif
        enddo chan
        if (iextra == 0) then
           write(iunit2) fix_tmp, data_tmp
        else
           write(iunit2) fix_tmp, data_tmp, extra_tmp
        endif
     enddo
     if (allocated(data_tmp)) deallocate(data_tmp)
     if (allocated(fix_tmp)) deallocate(fix_tmp)
     if (allocated(extra_tmp)) deallocate(extra_tmp)

     close(iunit)
     enddo peloop ! ipe
   close(iunit2)
 enddo ! satellite

  if (nobs /= nobs_max) then
      print *,'number of obs not what expected in get_satobs_data',nobs,nobs_max
      call stop2(92)
  end if
  if (nobsdiag /= nobs_maxdiag) then
      print *,'number of total diag obs not what expected in get_satobs_data',nobsdiag,nobs_maxdiag
      call stop2(92)
  end if

 end subroutine write_satobs_data


end module readsatobs
