Module ATMS_Spatial_Average_Mod
!
!
! abstract:  This routine reads BUFR format ATMS radiance 
!            (brightness temperature) files, spaatially 
!            averages the data using the AAPP averaging routines
!            and writes the data back into BUFR format
!
!
!
! Program history log:
!    2011-11-18   collard   - Original version
!    2017-07-13   yanqiu zhu - fix index bugs in subroutine ATMS_Spatial_Average
! 

  use kinds, only: r_kind,r_double,i_kind

  implicit none     


! Declare module level parameters
  real(r_double), parameter    :: Missing_Value=1.e11_r_double

contains 

  subroutine ATMS_Spatial_Average(Num_Obs, NChanl, FOV, Time, BT_InOut, &
       Scanline, Error_Status)

    implicit none
    
    ! Declare passed variables
    integer(i_kind) ,intent(in   ) :: Num_Obs, NChanl
    integer(i_kind) ,intent(in   ) :: Fov(num_obs)
    real(r_kind)    ,intent(in   ) :: Time(Num_Obs)
    real(r_kind)    ,intent(inout) :: BT_InOut(NChanl,Num_Obs)
    integer(i_kind) ,intent(  out) :: Scanline(Num_Obs)
    integer(i_kind) ,intent(  out) :: Error_Status

    ! Declare local parameters
    integer(i_kind), parameter :: atms1c_h_wmosatid=224
    integer(i_kind), parameter :: lninfile=15
    integer(i_kind), parameter :: max_fov=96
    real(r_kind), parameter    :: scan_interval = 8.0_r_kind/3.0_r_kind
    ! Maximum number of channels 
    integer(i_kind), parameter :: MaxChans = 22
    ! Minimum allowed BT as a function of channel number
    real(r_kind), parameter :: MinBT(MaxChans) = &
         (/ 120.0_r_kind, 120.0_r_kind, 190.0_r_kind, 190.0_r_kind, &
            200.0_r_kind, 200.0_r_kind, 200.0_r_kind, 190.0_r_kind, &
            190.0_r_kind, 180.0_r_kind, 180.0_r_kind, 180.0_r_kind, &
            190.0_r_kind, 200.0_r_kind, 200.0_r_kind, 120.0_r_kind, &
            120.0_r_kind, 120.0_r_kind, 150.0_r_kind, 170.0_r_kind, &
            180.0_r_kind, 190.0_r_kind /)
    ! Maximum allowed BT as a function of channel number
    real(r_kind), parameter :: MaxBT(MaxChans) = &
         (/ 320.0_r_kind, 320.0_r_kind, 300.0_r_kind, 300.0_r_kind, &
            300.0_r_kind, 270.0_r_kind, 250.0_r_kind, 240.0_r_kind, &
            240.0_r_kind, 250.0_r_kind, 250.0_r_kind, 270.0_r_kind, &
            280.0_r_kind, 290.0_r_kind, 300.0_r_kind, 320.0_r_kind, &
            320.0_r_kind, 300.0_r_kind, 300.0_r_kind, 300.0_r_kind, &
            300.0_r_kind, 300.0_r_kind /)
    

    ! Declare local variables
    character(30) :: Cline

    integer(i_kind) :: i, iscan, ifov, ichan, nchannels, wmosatid, version
    integer(i_kind) :: ios, max_scan, mintime
    integer(i_kind) :: nxaverage(nchanl), nyaverage(nchanl)
    integer(i_Kind) :: channelnumber(nchanl),qc_dist(nchanl)
    integer(i_kind), allocatable ::  scanline_back(:,:)

    real(r_kind) :: sampling_dist, beamwidth(nchanl) 
    real(r_kind) :: newwidth(nchanl), cutoff(nchanl),err(nchanl)
    real(r_kind), allocatable, target :: bt_image(:,:,:)

    Error_Status=0

    if (NChanl > MaxChans) then
       write(0,*) 'Unexpected number of ATMS channels: ',nchanl
       Error_Status = 1
       return
    end if

    ! Read the beamwidth requirements
    open(lninfile,file='atms_beamwidth.txt',form='formatted',status='old', &
         iostat=ios)
    if (ios /= 0) then
       write(*,*) 'Unable to open atms_beamwidth.txt'
       Error_Status=1
       return
    endif
    wmosatid=999
    read(lninfile,'(a30)',iostat=ios) Cline
    do while (wmosatid /= atms1c_h_wmosatid .and. ios == 0)
       do while (Cline(1:1) == '#')
          read(lninfile,'(a30)') Cline
       enddo
       read(Cline,*) wmosatid
       
       read(lninfile,'(a30)') Cline
       do while (Cline(1:1) == '#')
          read(lninfile,'(a30)') Cline
       enddo
       read(Cline,*) version
       
       read(lninfile,'(a30)') Cline
       do while (Cline(1:1) == '#')
          read(lninfile,'(a30)') Cline
       enddo
       read(Cline,*) sampling_dist
       
       read(lninfile,'(a30)') Cline
       do while (Cline(1:1) == '#')
          read(lninfile,'(a30)') Cline
       enddo
       read(Cline,*) nchannels
      
       read(lninfile,'(a30)') Cline
       if (nchannels > 0) then 
          do ichan=1,nchannels
             read(lninfile,'(a30)') Cline
             do while (Cline(1:1) == '#')
                read(lninfile,'(a30)') Cline
             enddo
             read(Cline,*) channelnumber(ichan),beamwidth(ichan), &
                  newwidth(ichan),cutoff(ichan),nxaverage(ichan), &
                  nyaverage(ichan), qc_dist(ichan)
          enddo
       end if
       read(lninfile,'(a30)',iostat=ios) Cline
    enddo
    if (wmosatid /= atms1c_h_wmosatid) then
       write(*,*) 'ATMS_Spatial_Averaging: sat id not matched in atms_beamwidth.dat'
       Error_Status=1
       return
    endif
    close(lninfile)
  

    ! Determine scanline from time
    MinTime = minval(Time)
    Scanline(:)   = nint((Time(1:Num_Obs)-MinTime)/Scan_Interval)+1
    Max_Scan=maxval(Scanline)
    
    allocate(BT_Image(Max_FOV,Max_Scan,nchanl))
    allocate(Scanline_Back(Max_FOV,Max_Scan))
    BT_Image(:,:,:) = 1000.0_r_kind
    
    ScanLine_Back(:,:) = -1
    do I=1,Num_Obs
       bt_image(FOV(I),Scanline(I),:)=bt_inout(:,I)
       Scanline_Back(FOV(I),Scanline(I))=I
    end do

!$omp parallel do schedule(dynamic,1) private(ichan,iscan,ios,ifov)
    do IChan=1,nchanl
    
       err(ichan)=0
   
       ! Set all scan positions to missing in a scanline if one is missing
       do iscan=1,max_scan
          if (any(bt_image(:,iscan,ichan) > 500.0_r_kind)) &
             bt_image(:,iscan,ichan)=1000.0_r_kind
       enddo

       ! If the channel number is present in the channelnumber array we should process it 
       ! (otherwise bt_inout just keeps the same value):
       do i=1,nchannels
          if (channelnumber(i) == ichan) then
             call modify_beamwidth ( max_fov, max_scan, bt_image(:,:,ichan), &
                  sampling_dist, beamwidth(i), newwidth(i), &
                  cutoff(i), nxaverage(i), nyaverage(i), &
                  qc_dist(i), MinBT(Ichan), MaxBT(IChan), IOS)

             if (IOS == 0) then
                do iscan=1,max_scan
                   do ifov=1,max_fov
                      if (Scanline_Back(IFov, IScan) > 0) &
                        bt_inout(ichan,Scanline_Back(IFov, IScan)) = &
                        BT_Image(ifov,iscan,ichan)
                   end do
                end do
             else
                err(ichan)=1
             end if
          end if
       end do
    end do

    do ichan=1,nchanl
      if(err(ichan) >= 1)then
         error_status = 1
         return
      end if
    end do

    deallocate(BT_Image, Scanline_Back)
    
end subroutine ATMS_Spatial_Average



subroutine modify_beamwidth ( nx, ny, image, sampling_dist,& 
     beamwidth, newwidth, mtfcutoff, nxaverage, nyaverage, qc_dist, &
     Minval, MaxVal, Error)
     
!-----------------------------------------
! Name: $Id$
!
! Purpose:
!   Manipulate the effective beam width of an image. For example, convert ATMS
!   to AMSU-A-like resolution while reducing the noise.
!
! Method:
!   1) Pad the image to a power of 2 in each dimension.
! If FFT technique is to be used then: 
!   2) Assuming Gaussian beam shapes, calcluate the input and output Modulation
!      Transfer Functions (MTF).
!   3) FFT image to frequency domain (2-D).
!   4) Multiply by output MTF divided by input MTF. If a cut-off is specified
!      (when attempting to make the beam width narrower), attenuate further
!      by an exponential function - factor of 2 at the cutoff. 
!   5) FFT back to image domain 
! Finally,
!   6) Over-write the input image, with averaging if requested.
!
! COPYRIGHT
!    This software was developed within the context of the EUMETSAT Satellite
!    Application Facility on Numerical Weather Prediction (NWP SAF), under the
!    Cooperation Agreement dated 1 December 2006, between EUMETSAT and the
!    Met Office, UK, by one or more partners within the NWP SAF. The partners
!    in the NWP SAF are the Met Office, ECMWF, KNMI and MeteoFrance.
!
!    Copyright 2010, EUMETSAT, All Rights Reserved.
!
! History:
! Version    Date     Comment
!
!  1.0   22/07/2010   N.C.Atkinson
!  1.1   21/11/2011   Convert to f90. A. Collard
!
! Code Description:
!   FORTRAN 77, following AAPP standards
!
! Declarations:


      implicit none
! Parameters
      integer(i_kind), parameter :: nxmax=128  !Max number of spots per scan line
      integer(i_kind), parameter :: nymax=8192 !Max number of lines. Allows 6hrs of ATMS.

! Arguments
      integer(i_kind), intent(in)  :: nx, ny         !Size of image
      real(r_kind), intent(inout)  :: image(nx,ny)   !BT or radiance image
      real(r_kind), intent(in)     :: sampling_dist  !typically degrees
      real(r_kind), intent(in)     :: beamwidth      !ditto
      real(r_kind), intent(in)     :: newwidth       !ditto
      real(r_kind), intent(in)     :: mtfcutoff      !0.0 to 1.0
      integer(i_kind), intent(in)  :: nxaverage      !Number of samples to average (or zero)
      integer(i_kind), intent(in)  :: nyaverage      !Number of samples to average (or zero)
      integer(i_kind), intent(in)  :: qc_dist        !Number of samples around missing data to set to 
      real(r_kind), intent(in)     :: maxval         !BTs above this are considered missing
      real(r_kind), intent(in)     :: minval         !BTs below this are considered missing
      integer(i_kind), intent(out) :: Error          !Error Status
       
! Local variables
      integer(i_kind) :: nxpad, nypad, dx, dy
      integer(i_kind) :: i,j,k,ix,iy, ii, jj
      integer(i_kind) :: ifirst
      integer(i_kind) :: xpow2, ypow2
      integer(i_kind) :: nxav2, nyav2, naverage
      integer(i_kind) :: deltax, minii, maxii, minjj, maxjj
      real(r_kind), allocatable :: mtfxin(:),mtfxout(:)
      real(r_kind), allocatable :: mtfyin(:),mtfyout(:)
      real(r_kind) :: mtfin,mtfout,mtf_constant
      real(r_kind), allocatable :: mtfpad(:,:)
      real(r_kind), allocatable :: imagepad(:,:)
      real(r_kind) :: f,df,factor
      real(r_kind) :: PI, LN2, LNcsquared
      logical :: missing
      logical, allocatable :: gooddata_map(:,:)


! End of declarations
!-----------------------------------------
      
      PI = 4.0_r_kind*atan(1.0_r_kind)
      LN2 = LOG(2.0_r_kind)
      MTF_Constant=-(PI/(2*sampling_dist))**2/LN2
      if (mtfcutoff > 0.0_r_kind) LNcsquared = log(mtfcutoff)**2
      nxav2 = nxaverage/2
      nyav2 = nyaverage/2
      naverage = nxaverage*nyaverage
      Error = 0

!1) Pad the image up to the nearest power of 2 in each dimension, by reversing
!the points near the edge.

      xpow2 = int(log(nx*1.0_r_kind)/LN2 + 1.0_r_kind)
      ypow2 = int(log(ny*1.0_r_kind)/LN2 + 1.0_r_kind)
      nxpad = 2**xpow2
      nypad = 2**ypow2
      dx = (nxpad - nx)/2
      dy = (nypad - ny)/2

      if (nxpad > nxmax) then
         write(*,*) 'ATMS_Spatial_Average: nx too large, maximum allowed value is ',nxmax-1
         Error = 1
         return
      end if
      
      if (nypad > nymax) then
         write(*,*) 'ATMS_Spatial_Average: ny too large, maximum allowed value is ',nymax-1
         Error = 1
         return
      end if

      allocate(mtfxin(nxpad),mtfxout(nxpad))
      allocate(mtfyin(nypad),mtfyout(nypad))
      allocate(mtfpad(nxpad,nypad))
      allocate(imagepad(nxpad,nypad))
      allocate(gooddata_map(nxmax,nymax))

!Loop over scan positions
      do j=dy+1,dy+ny
        do i=dx+1,dx+nx
          if (image(i-dx,j-dy) < minval) &
               image(i-dx,j-dy) = minval - 1.0_r_kind
          if (image(i-dx,j-dy) > maxval ) &
               image(i-dx,j-dy) = maxval + 1.0_r_kind
          imagepad(i,j) = image(i-dx,j-dy)   !Take a copy of the input data
          gooddata_map(i,j) = .true.   ! Initialised for step 6)
        enddo

!Interpolate missing points in the along-track direction

        ifirst = -1
        missing = .false.
        
        do i=dx+1,dx+nx
          if (.not.missing) then
            if (imagepad(i,j) >= minval .and. imagepad(i,j) <= maxval) then
              ifirst = i
            else
              missing = .true.
            endif
          else
            if (imagepad(i,j) >= minval .and. imagepad(i,j) <= maxval) then  !First good point 
                                                                             ! after missing
               missing = .false.
               if (ifirst == -1) then
                  do k=dx+1,i-1
                     imagepad(k,j) = imagepad(i,j)      !Constant
                  enddo
               else
                  do k=ifirst+1,i-1
                     factor = (i-k)*1.0_r_kind/(i-ifirst)      !Interpolate
                     imagepad(k,j) = imagepad(ifirst,j)*factor + &
                          imagepad(i,j)*(1.0_r_kind-factor)
                  enddo
               endif
            endif
          endif
        enddo
        if (missing) then         !Last scan is missing
          if (ifirst >= 1) then
            do k=ifirst+1,dx+nx
              imagepad(k,j) = imagepad(ifirst,j)     !Constant
            enddo
          endif
        endif          

!Continue padding the edges

        do i=1,dx
          imagepad(i,j) = imagepad(dx+dx+2-i,j)
        enddo
        do i=nx+dx+1,nxpad
          imagepad(i,j) = imagepad(nx+dx+nx+dx-i,j)
        enddo
     enddo
     do j=1,dy
        do i=1,nxpad
           imagepad(i,j) = imagepad(i,dy+dy+2-j)
        enddo
     enddo
     do j=ny+dy+1,nypad
        do i=1,nxpad
           imagepad(i,j) = imagepad(i,ny+dy+ny+dy-j)
        enddo
     enddo

!2) Compute the MTF modifications. Assume beams are Gaussian.

      if (newwidth > 0) then
        df = 1.0_r_kind/nxpad
        do i=1,nxpad/2+1
          f = df*(i-1)      !DC to Nyquist
          mtfxin(i) = exp(MTF_Constant*(f*beamwidth)**2)
          mtfxout(i) = exp(MTF_Constant*(f*newwidth)**2)
          if (i > 1 .and. i < nxpad/2+1) then
            mtfxin(nxpad-i+2) = mtfxin(i)
            mtfxout(nxpad-i+2) = mtfxout(i)
          endif
        enddo
        df = 1.0_r_kind/nypad
        do i=1,nypad/2+1
          f = df*(i-1)      !DC to Nyquist
          mtfyin(i) = exp(MTF_Constant*(f*beamwidth)**2)
          mtfyout(i) = exp(MTF_Constant*(f*newwidth)**2)
          if (i > 1 .and. i < nypad/2+1) then
            mtfyin(nypad-i+2) = mtfyin(i)
            mtfyout(nypad-i+2) = mtfyout(i)
          endif
        enddo
        do i=1,nxpad
          do j=1,nypad
            mtfin = mtfxin(i)*mtfyin(j)
            mtfout = mtfxout(i)*mtfyout(j)
            if (mtfcutoff > 0.0_r_kind) then
              mtfpad(i,j) = (mtfout * &
                exp(-LN2/LNcsquared*(log(mtfout))**2))/mtfin
            else
              mtfpad(i,j) = mtfout/mtfin
            endif
          enddo
        enddo

!3) Fourier transform, line by line then column by column.
!After each FFT, points 1 to nxpad/2+1 contain the real part of the spectrum,
!the rest contain the imaginary part in reverse order.

        do j=1,nypad
           call sfftcf(imagepad(:,j),nxpad,xpow2)
        enddo

        do i=1,nxpad
           call sfftcf(imagepad(i,:),nypad,ypow2)

!4) Multiply the spectrum by the MTF factor
           do j=1,nypad
              imagepad(i,j) = imagepad(i,j)*mtfpad(i,j)
           enddo
        enddo

!5) Inverse Fourier transform, column by column then line by line 

        do i=1,nxpad
          call sfftcb(imagepad(i,:),nypad,ypow2)
        enddo

        do j=1,nypad
          call sfftcb(imagepad(:,j),nxpad,xpow2)
        enddo
     endif   !New width is specified

!6) Reset missing values in gooddata_map, based on qc_dist and the values 
!   in the input image array

     ! Set the ends of the image to missing in the along track direction
     ! (doing the same across track will remove too much data)
     gooddata_map(:,1:min(qc_dist,ny))=.false.
     gooddata_map(:,max(ny-qc_dist+1,1):ny)=.false.
     
     do j=1,ny
        do i=1,nx
           if (image(i,j) <= minval .or. image(i,j) >= maxval ) then
              minjj=max(j+dy-qc_dist,1)
              maxjj=min(j+dy+qc_dist,nymax)
              do jj=minjj,maxjj
                 deltax=int(sqrt(real(qc_dist**2 - (jj-j-dy)**2 )))
                 minii=max(i+dx-deltax,1)
                 maxii=min(i+dx+deltax,nxmax)
                 do ii=minii,maxii
                    gooddata_map(ii,jj)=.false.
                 end do
              end do
           end if
        end do
     end do

!7) Over-write the input image (points that are not missing)

     do j=1,ny
        do i=1,nx
           if (gooddata_map(i+dx,j+dy)) then
              if (nxav2 == 0.0_r_kind .and. nyav2 == 0) then
                 image(i,j) = imagepad(i+dx,j+dy)
              else
                 image(i,j) = 0.0_r_kind             !Do averaging
                 do ix = -nxav2,nxav2
                    do iy = -nyav2,nyav2
                       image(i,j) = image(i,j) + imagepad(i+dx+ix,j+dy+iy)
                    enddo
                 enddo
                 image(i,j) = image(i,j)/naverage
              endif
           else
              image(i,j) = missing_value
           end if
        enddo
     enddo

!8) Deallocate arrays

     deallocate(mtfxin,mtfxout)
     deallocate(mtfyin,mtfyout)
     deallocate(mtfpad)
     deallocate(imagepad)
     deallocate(gooddata_map)

     return
   end subroutine modify_beamwidth

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!  A real-valued, in place, split-radix FFT program
!  Real input and output in data array X
!  Length is N = 2 ** M
!  Decimation-in-time, cos/sin in second loop
!  Output in order:
!         [ Re(0), Re(1), ..., Re(N/2), Im(N/2-1), ..., Im(1) ]
!
!  This FFT computes
!     X(k) = sum_{j=0}^{N-1} x(j)*exp(-2ijk*pi/N)
!
!
!  H.V. Sorensen, Rice University, Oct. 1985
!
!  Reference:  H.V. Sorensen, D.L. Jones, M.T. Heideman, & C.S. Burrus;
!              Real-Valued Fast Fourier Transform Algorithms; IEEE
!              Trans. Acoust., Speech, Signal Process.; Vol ASSP-35,
!              June 1987, pp. 849-863.
!
!  This code was originally named RVFFT.
!
!  History:
!   21/11/2011 Converted to something resembling f90.   A.Collard
!
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
      subroutine sfftcf( x, n, m )

      implicit none

! ... Parameters ...
      real(r_kind), parameter :: sqrt2 = 1.4142135623730950488_r_kind
      real(r_kind), parameter :: twopi = 6.2831853071795864769_r_kind

! ... Scalar arguments ...
      integer(i_kind), intent(in) :: n, m
! ... Array arguments ...
      real(r_kind), intent(inout) ::  x(n)
! ... Local scalars ...
      integer(i_kind)  j, i, k, is, id, i0, i1, i2, i3, i4, i5, i6, i7, i8
      integer(i_kind)  n1, n2, n4, n8
      real(r_kind)  xt, r1, t1, t2, t3, t4, t5, t6
      real(r_kind)  a, a3, e, cc1, ss1, cc3, ss3
!
! ... Exe. statements ...
!
      if ( n == 1 ) return
!
      j = 1
      n1 = n - 1
      do 104, i = 1, n1
         if ( i < j ) then
            xt = x(j)
            x(j) = x(i)
            x(i) = xt
         end if
         k = n / 2
 102     do while (k < j) 
            j = j - k
            k = k / 2
         end do
         j = j + k
 104  continue
! 
      is = 1
      id = 4
      loop1:do
         do 60, i0 = is, n, id
            i1 = i0 + 1
            r1 = x(i0)
            x(I0) = r1 + x(i1)
            x(I1) = r1 - x(i1)
 60      continue
         is = 2 * id - 1
         id = 4 * id
         if ( is >= n ) exit loop1
      end do loop1
!
      n2 = 2
      do 10, k = 2, m
         n2 = n2 * 2
         n4 = n2 / 4
         n8 = n2 / 8
         e = twopi / n2
         is = 0
         id = n2 * 2
         loop2: do
            do 38, i = is, n-1, id
               i1 = i + 1
               i2 = i1 + n4
               i3 = i2 + n4
               i4 = i3 + n4
               t1 = x(i4) + x(i3)
               x(i4) = x(i4) - x(i3)
               x(i3) = x(i1) - t1
               x(i1) = x(i1) + t1
               if ( n4 == 1 ) cycle
               i1 = i1 + n8
               i2 = i2 + n8
               i3 = i3 + n8
               i4 = i4 + n8
               t1 = ( x(i3) + x(i4) ) / sqrt2
               t2 = ( x(i3) - x(i4) ) / sqrt2
               x(i4) = x(i2) - t1
               x(i3) = - x(i2) - t1
               x(i2) = x(i1) - t2
               x(i1) = x(i1) + t2
 38         continue
            is = 2 * id - n2
            id = 4 * id
            if ( is >=  n ) exit loop2
         end do loop2
         a = e
         do 32, j = 2, n8
            a3 = 3 * a
            cc1 = cos(a)
            ss1 = sin(a)
            cc3 = cos(a3)
            ss3 = sin(a3)
            a = j * e
            is = 0
            id = 2 * n2
            loop3: do
               do 30, i = is, n-1, id
                  i1 = i + j
                  i2 = i1 + n4
                  i3 = i2 + n4
                  i4 = i3 + n4
                  i5 = i + n4 - j + 2
                  i6 = i5 + n4
                  i7 = i6 + n4
                  i8 = i7 + n4
                  t1 = x(i3) * cc1 + x(i7) * ss1
                  t2 = x(i7) * cc1 - x(i3) * ss1
                  t3 = x(i4) * cc3 + x(i8) * ss3
                  t4 = x(i8) * cc3 - x(i4) * ss3
                  t5 = t1 + t3
                  t6 = t2 + t4
                  t3 = t1 - t3
                  t4 = t2 - t4
                  t2 = x(i6) + t6
                  x(i3) = t6 - x(i6)
                  x(i8) = t2
                  t2 = x(i2) - t3
                  x(i7) = - x(i2) - t3
                  x(i4) = t2
                  t1 = x(i1) + t5
                  x(i6) = x(i1) - t5
                  x(i1) = t1
                  t1 = x(i5) + t4
                  x(i5) = x(i5) - t4
                  x(i2) = t1
 30            continue
               is = 2 * id - n2
               id = 4 * id
               if ( is >= n ) exit loop3
            end do loop3
 32      continue
 10   continue
      return
!
! ... End of subroutine SFFTCF ...
!
   end subroutine sfftcf

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!  A real-valued, in place, split-radix IFFT program
!  Hermitian symmetric input and real output in array X
!  Length is N = 2 ** M
!  Decimation-in-frequency, cos/sin in second loop
!  Input order:
!         [ Re(0), Re(1), ..., Re(N/2), Im(N/2-1), ..., Im(1) ]
!
!  This FFT computes
!     x(j) = (1/N) * sum_{k=0}^{N-1} X(k)*exp(2ijk*pi/N)
!
!
!  H.V. Sorensen, Rice University, Nov. 1985
!
!  Reference:  H.V. Sorensen, D.L. Jones, M.T. Heideman, & C.S. Burrus;
!              Real-Valued Fast Fourier Transform Algorithms; IEEE
!              Trans. Acoust., Speech, Signal Process.; Vol ASSP-35,
!              June 1987, pp. 849-863.
!
!  This code was originally named IRVFFT.
!
!  History:
!   21/11/2011 Converted to something resembling f90.   A.Collard
!
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
      subroutine sfftcb( x, n, m )

      use kinds, only: r_kind,r_double,i_kind

      implicit none

! ... Parameters ...
      real(r_kind), parameter :: sqrt2 = 1.4142135623730950488_r_kind
      real(r_kind), parameter :: twopi = 6.2831853071795864769_r_kind

! ... Scalar arguments ...
      integer(i_kind), intent(in) :: n, m
! ... Array arguments ...
      real(r_kind), intent(inout) ::  x(n)
! ... Local scalars ...
      integer(i_kind)  j, i, k, is, id, i0, i1, i2, i3, i4, i5, i6, i7, i8
      integer(i_kind)  n1, n2, n4, n8
      real(r_kind)  xt, r1, t1, t2, t3, t4, t5
      real(r_kind)  a, a3, e, cc1, ss1, cc3, ss3
!
! ... Exe. statements ...
!
      if ( n == 1 ) return
!
      n2 = 2 * n
      do 10, k = 1, m-1
         is = 0
         id = n2
         n2 = n2 / 2
         n4 = n2 / 4
         n8 = n4 / 2
         e = twopi / n2
         loop1: do
            do 15, i = is, n-1, id
               i1 = i + 1
               i2 = i1 + n4
               i3 = i2 + n4
               i4 = i3 + n4
               t1 = x(i1) - x(i3)
               x(i1) = x(i1) + x(i3)
               x(i2) = 2 * x(i2)
               x(i3) = t1 - 2 * x(i4)
               x(i4) = t1 + 2 * x(i4)
               if ( n4 == 1 ) cycle
               i1 = i1 + n8
               i2 = i2 + n8
               i3 = i3 + n8
               i4 = i4 + n8
               t1 = ( x(i2) - x(i1) ) / sqrt2
               t2 = ( x(i4) + x(i3) ) / sqrt2
               x(i1) = x(i1) + x(i2)
               x(i2) = x(i4) - x(i3)
               x(i3) = 2 * ( - t2 - t1 )
               x(i4) = 2 * ( -t2 + t1 )
 15         continue
            is = 2 * id - n2
            id = 4 * id
            if ( is >= n-1 ) exit loop1
         end do loop1
         a = e
         do 20, j = 2, n8
            a3 = 3 * a
            cc1 = cos(a)
            ss1 = sin(a)
            cc3 = cos(a3)
            ss3 = sin(a3)
            a = j * e
            is = 0
            id = 2 * n2
            loop2: do
 40            do 30, i = is, n-1, id
                  i1 = i + j
                  i2 = i1 + n4
                  i3 = i2 + n4
                  i4 = i3 + n4
                  i5 = i + n4 - j + 2
                  i6 = i5 + n4
                  i7 = i6 + n4
                  i8 = i7 + n4
                  t1 = x(i1) - x(i6)
                  x(i1) = x(i1) + x(i6)
                  t2 = x(i5) - x(i2)
                  x(i5) = x(i2) + x(i5)
                  t3 = x(i8) + x(i3)
                  x(i6) = x(i8) - x(i3)
                  t4 = x(i4) + x(i7)
                  x(i2) = x(i4) - x(i7)
                  t5 = t1 - t4
                  t1 = t1 + t4
                  t4 = t2 - t3
                  t2 = t2 + t3
                  x(i3) = t5 * cc1 + t4 * ss1
                  x(i7) = - t4 * cc1 + t5 * ss1
                  x(i4) = t1 * cc3 - t2 * ss3
                  x(i8) = t2 * cc3 + t1 * ss3
 30            continue
               is = 2 * id - n2
               id = 4 * id
               if ( is >= n-1 ) exit loop2
             end do loop2
 20      continue
 10   continue
!
      is = 1
      id = 4
      loop3: do
         do 60, i0 = is, n, id
            i1 = i0 + 1
            r1 = x(i0)
            x(i0) = r1 + x(i1)
            x(i1) = r1 - x(i1)
 60      continue
         is = 2 * id - 1
         id = 4 * id
         if ( is >= n ) exit loop3
      end do loop3
!
      j = 1
      n1 = n - 1
      do 104, i = 1, n1
         if ( i < j ) then
            xt = x(j)
            x(j) = x(i)
            x(i) = xt
         end if
         k = n / 2
         do while (k < j )         
            j = j - k
            k = k / 2
         end do
         j = j + k
 104  continue
      xt = 1.0_r_kind / float( n )
      do 99, i = 1, n
         x(i) = xt * x(i)
 99   continue
      return
!
! ... End of subroutine SFFTCB ...
! 
      end subroutine sfftcb

end module ATMS_Spatial_Average_Mod
