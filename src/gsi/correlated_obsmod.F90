!BOI

!  !TITLE: Correlated\_ObsMod: Inter-channel Observation Correlation Module

!  !AUTHORS: Ricardo Todling

!  !AFFILIATION: Global Modeling and Assimilation Office, NASA/GSFC, Greenbelt, MD 20771

!  !DATE: 13 April 2014

!  !INTRODUCTION: Overview
#ifdef __PROTEX__

This module introduces the ability for GSI to account for inter-channel
correlated errors for radiance observations. It assumes an offline estimate of
an observation error covariance for a given instrument is available. 

At GMAO, the offline estimation of the error covariances required by this module
is performed by a FORTRAN program that reads the GSI-diag files and performs 
statistics on the observation-minus-background and observation-minus-analysis
residuals, following the so-called Desroziers approach (e.g., Desroziers et al.
2005; Q. J. R. Meteorol. Soc., 131, 3385-3396).

At NCEP, the offline estimation of the error covariances can be computed
by the cov_calc module, located in util/Correlated_Obs.  This module is also
based on the Desroziers method.

This module defines the so-called Obs\_Error\_Cov.

As Met\_Guess and other like-modules, the idea is for this module to define nearly 
opaque object. However, so far, we have had no need to add inquire-like functions - that
is, no code outside this code needs to what what is inside GSI\_Obs\_Error\_Cov. 
So far, only very general `methods'' are made public from this module, these
being, 

\begin{verbatim}
public :: corr_ob_initialize
public :: corr_ob_amiset
public :: corr_ob_scale_jac
public :: corr_ob_finalize
\end{verbatim}

and never the variables themselves; the only exception being the GSI\_MetGuess\_Bundle itself 
(until it is no longer treated as a common-block).  Some of the public methods above are 
overloaded and all have internal interfaces (name of which appears in the index of this protex 
document. It should be a rule here that any new routine to be made public should
have a declared interface procedure.

\begin{center}
\fbox{Obs\_Error\_Cov is defined via the {\it correlated\_observations} table in a resource file}
\end{center}

\underline{Defining Observation Error Covariance Models} is done via the table {\it correlated\_observations},
usually embedded in the {\it anavinfo} file. An example of such table follows:
\begin{verbatim}
correlated_observations::
! isis       method    type    cov_file
  airs_aqua     1      60.   ice     airs_rcov.bin
  airs_aqua     1      60.   land    airs_rcov.bin
  airs_aqua     1      60.   sea     airs_rcov.bin
  airs_aqua     1      60.   snow    airs_rcov.bin
  airs_aqua     1      60.   mixed   airs_rcov.bin
# cris_npp      1     -99.   snow    cris_rcov.bin
# cris_npp      1     -99.   land    cris_rcov.bin
# cris_npp      1     -99.   sea     cris_rcov.bin
  iasi_metop-a  2      0.12  snow    iasi_sea_rcov.bin
  iasi_metop-a  2      0.22  land    iasi_land_rcov.bin
  iasi_metop-a  2      0.05  sea     iasi_sea_rcov.bin
  iasi_metop-a  2      0.12  ice     iasi_sea_rcov.bin
  iasi_metop-a  2      0.12  mixed   iasi_sea_rcov.bin
# ssmis_f17     1     -99.   mixed   ssmis_rcov.bin
# ssmis_f17     1     -99.   land    ssmis_rcov.bin
# ssmis_f17     1     -99.   sea     ssmis_rcov.bin

::
\end{verbatim}
Notice that the covariance can be supplied for all five surface types,
namely,  ice, snow, mixed, land, and sea. However, they can be made the same, by simply
pointing the different types to the same file. In the example above, only AIRS and
IASI from Metop-A are being specially handled by this module. In the case of
AIRS, no distinction is made among the different types of surfaces, whereas 
in the case of IASI, a distinction is made between land and sea, with everything
else being treated as sea.  It is not necessary to specify a covariance file for
each surface type.

The instrument name is the same as it would be in the satinfo file.

As usual, this table follows INPAK/ESMF convention, begining with a name
(correlated\_observations), followed by double colons (::) to open the table and 
ending with double colons.  Any line starting with an exclamation mark or a pound sign 
is taken as a comment.

The current {\it correlated\_observations} table has four columns defined as follows:

\begin{verbatim}
Column 1: isis   - refers to instrument/platform type (follows, typical GSI nomenclature)
Column 2: method - specify different possibilities for handling the corresponding 
          cov(R) at present:
          <0 - reproduces GSI to within roundoff (for testing only)
           0 - diag of estimated R only
           1 - use full R covariance, but use satinfo errors in quality control
           2 - use full R covariance, and use the diagonal of R in quality control
           3 - as (2), but with situational dependent obs error inflation
Column 3: type - determines whether to apply covariance over ocean, land, ice, snow or mixed FOVs
Column 4: cov_file - name of file holding estimate of error covariance for the
                     instrument specified in column 1
\end{verbatim}

#endif
!EOI

!-------------------------------------------------------------------------
!  NASA/GSFC, Global Modeling and Assimilation Office, Code 610.1, GMAO  !
!-------------------------------------------------------------------------
!BOP
!  
! !MODULE: correlated_obsmod -- inter-channel correlation ob error capability
!
! !INTERFACE:

module correlated_obsmod
!
! !DESCRIPTION: Module to handle options and components related to
!               specifying inter-channel correlation observation error 
!               covariances.
!
!               This uses wired-in type arrays to hold error covariances
!               for different instruments. This should be revisited in
!               the future.
!
! !USES:

use kinds, only: i_kind, r_single, r_kind, r_double
use constants, only: zero,one
use mpimod, only: mype
use timermod, only: timer_ini, timer_fnl
use mpeu_util, only: gettablesize
use mpeu_util, only: gettable
use mpeu_util, only: die
use mpeu_util, only: luavail
use gsi_io, only: verbose
private
save

! !PUBLIC MEMBER FUNCTIONS:

public corr_ob_initialize
public corr_ob_finalize
public corr_ob_scale_jac
public corr_ob_amiset
public idnames
public ObsErrorCov
public GSI_BundleErrorCov
public corr_oberr_qc

! !METHOD OVERLOADING:

interface corr_ob_initialize; module procedure ini_; end interface
interface corr_ob_amiset; module procedure amIset_; end interface
interface corr_oberr_qc; module procedure upd_varqc_; end interface
interface corr_ob_scale_jac; module procedure scale_jac_; end interface
interface corr_ob_finalize; module procedure fnl_; end interface

! !REVISION HISTORY:
!
!   15Apr2014 Todling  Initial code.
!   19Dec2014 W. Gu, Add a new interface corr_oberr_qc to update the prescribed obs errors in satinfo for instrments 
!                    accounted for the correlated R-covariance.
!
!EOP
!-------------------------------------------------------------------------
!BOC

integer(i_kind),parameter::MAXSTR=256
character(len=MAXSTR),allocatable :: instruments(:)
character(len=MAXSTR),allocatable :: idnames(:)

integer :: ninstr=-1   ! single instrument for now
logical :: iamroot_

! !PRIVATE TYPES:

type ObsErrorCov
     character(len=40) :: name                        ! R covariance name
     character(len=20) :: instrument                  ! instrument
     integer(i_kind)   :: nch_active=-1               ! active channels
     integer(i_kind)   :: nctot=-1                    ! total number of channels (active+passive)
     integer(i_kind)   :: method    =-1               ! define method of computation
     character(len=20) :: mask      ='sea'            ! Apply covariance for profiles over sea
     integer(i_kind),pointer :: indxR(:)   =>NULL()   ! indexes of active channels
     real(r_kind),   pointer :: R(:,:)     =>NULL()   ! nch_active x nch_active
     real(r_kind),   pointer :: UT(:,:)=>NULL()       ! Upper triangle of R^-1 subset
     real(r_kind),   pointer :: UTfull(:,:)=>NULL()   ! Upper triangle of decomp of R^-1

end type

! !PUBLIC TYPES:

type(ObsErrorCov),pointer :: GSI_BundleErrorCov(:)

! strictly internal quantities
character(len=*),parameter :: myname='correlated_obsmod'
logical :: initialized_=.false.
integer(i_kind),parameter :: methods_avail(5)=(/-1, & ! do nothing
                                                 0, & ! use dianonal of estimate(R)
                                                 1, & ! use full est(R), satinfo errors in qc
                                                 2, & ! use full est(R), diag(R) in qc
                                                 3/)  ! same as 2, but with SDOEI
contains

!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  ini_ --- Initialize info about correlated obs (read resource table)
!
! !INTERFACE:
!
subroutine ini_ (miter,iamroot)
! !USES:
use mpeu_util, only: die
implicit none
! !INPUT PARAMETERS:
   integer(i_kind), intent(in):: miter
   logical,optional,intent(in) :: iamroot 

! !DESCRIPTION: Define parameters and setting for handling correlated
!               observation errors via resouce file reading.
!
! !REVISION HISTORY:
!   2014-04-13  todling  initial code
!
! !REMARKS:
!   language: f90
!   machine:  discover
!
! !AUTHOR:
!   Ricardo Todling  org: gmao      date: 2014-04-13
!
!EOP
!-------------------------------------------------------------------------
!BOC
character(len=*),parameter:: rcname='anavinfo'  ! filename should have extension
character(len=*),parameter:: tbname='correlated_observations::'
integer(i_kind) luin,ii,ntot,nrows,method
character(len=MAXSTR),allocatable,dimension(:):: utable
character(len=20) instrument, mask
character(len=30) filename
character(len=*),parameter::myname_=myname//'*ini_'

if(initialized_) return

iamroot_=mype==0
if(present(iamroot)) iamroot_=iamroot 

! load file
luin=luavail()
open(luin,file=rcname,form='formatted')

! Scan file for desired table first
! and get size of table
call gettablesize(tbname,luin,ntot,nrows)
if(nrows==0) then
   close(luin)
   return
endif
ninstr=nrows

! Get contents of table
allocate(utable(ninstr),instruments(ninstr),idnames(ninstr))
call gettable(tbname,luin,ntot,ninstr,utable)

! release file unit
close(luin)

allocate(GSI_BundleErrorCov(ninstr))

! Retrieve each token of interest from table and define
! variables participating in state vector

! Count variables first
if(iamroot_) write(6,*) myname_,': Correlated-Obs for the following instruments'
do ii=1,ninstr
   read(utable(ii),*) instrument, method, mask, filename ! if adding col to table leave fname as last
   instruments(ii) = trim(instrument)
   idnames(ii) = trim(instrument)//':'//trim(mask)
   if(iamroot_) then
      write(6,'(1x,2(a,1x),1x,f7.2,1x,a)') trim(instrument), trim(mask), method, trim(filename)
   endif
!  check method validity
   if(ALL(methods_avail/=method)) then
     call die(myname_,' invalid choice of method, aborting')
   endif
   call set_(trim(instrument),trim(filename),mask,method,GSI_BundleErrorCov(ii),miter)
enddo

! release table
deallocate(utable)

end subroutine ini_
!EOC

!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  set_ --- set error covariances for different instruments
!
! !INTERFACE:
!
subroutine set_(instrument,fname,mask,method,ErrorCov,miter)
implicit none

! !INPUT PARAMETERS:

character(len=*),intent(in) :: instrument  ! name of instrument
character(len=*),intent(in) :: fname       ! filename holding cov(R)
character(len=*),intent(in) :: mask        ! land/sea/etc mask
integer,intent(in):: method                ! method to apply when using this cov(R)
integer,intent(in):: miter                 ! if =0 then just check for Rcov file, don't read
type(ObsErrorCov) :: ErrorCov              ! cov(R) for this instrument

! !DESCRIPTION: Given basic information on the instrument type
!               this routine reads an available estimate
!               of the corresponding fully-correlated error
!               covariance and fills the FORTRAN type defined
!               as ObsErrorCov.
!
! !REVISION HISTORY:
!   2014-04-13  todling  initial code
!   2014-08-06  todling  platform-specific correlated obs handle
!
! !REMARKS:
!   language: f90
!   machine:  discover
!
! !AUTHOR:
!   Ricardo Todling  org: gmao      date: 2014-04-13
!
!EOP
!-------------------------------------------------------------------------
!BOC

character(len=*),parameter :: myname_=myname//'*set'
integer(i_kind) nch_active,lu,ii,ioflag,iprec,nctot

real(r_single),allocatable, dimension(:,:) :: readR4  ! nch_active x nch_active x ninstruments
real(r_double),allocatable, dimension(:,:) :: readR8  ! nch_active x nch_active x ninstruments
real(r_kind),allocatable, dimension(:) :: diag
   logical print_verbose

   print_verbose=.false.
   if(verbose .and. iamroot_)print_verbose=.true.
   ErrorCov%instrument = trim(instrument)
   ErrorCov%mask = trim(mask)
   ErrorCov%name = trim(instrument)//':'//trim(mask)
   ErrorCov%method = method

   lu = luavail()
   open(lu,file=trim(fname),convert='little_endian',form='unformatted')
   read(lu,IOSTAT=ioflag) nch_active, nctot, iprec
   if(ioflag/=0) call die(myname_,' failed to read nch from '//trim(fname))
   if (miter==0) return
   ErrorCov%nch_active = nch_active
   ErrorCov%nctot = nctot
   call create_(nch_active,ErrorCov)

!  Read GSI-like channel numbers used in estimating R for this instrument
   read(lu,IOSTAT=ioflag) ErrorCov%indxR
   if(ioflag/=0) call die(myname_,' failed to read indx from '//trim(fname))

!  Read estimate of observation error covariance
   if(iprec==4) then
     allocate(readR4(nch_active,nch_active))
     read(lu,IOSTAT=ioflag) readR4
     if(ioflag/=0) call die(myname_,' failed to read R from '//trim(fname))
     ErrorCov%R = readR4
     deallocate(readR4)
   endif
   if(iprec==8) then
     allocate(readR8(nch_active,nch_active))
     read(lu,IOSTAT=ioflag) readR8
     if(ioflag/=0) call die(myname_,' failed to read R from '//trim(fname))
     ErrorCov%R = readR8
     deallocate(readR8)
   endif

!  Done reading file
   close(lu)

!  If method<0 there is really nothing else to do
!  ----------------------------------------------
   if (method<0) then
      initialized_=.true.
      return
   endif
   if (print_verbose) then
       allocate(diag(nch_active))
       do ii=1,nch_active
          diag(ii)=ErrorCov%R(ii,ii)
       enddo
       write(6,'(2a)') 'Rcov(stdev) for instrument: ', trim(ErrorCov%name)
       write(6,'(9(1x,es10.3))') sqrt(diag)
       write(6,'(3a)') 'Channels used in estimating Rcov(', trim(ErrorCov%name), ')'
       write(6,'(12(1x,i5))') ErrorCov%indxR
       deallocate(diag)
   endif

!  Now decompose R
   call solver_(ErrorCov)

   if (print_verbose .and. ErrorCov%method>=0) then
       allocate(diag(nch_active))
       do ii=1,nch_active
          diag(ii)=ErrorCov%R(ii,ii)
       enddo
       write(6,'(3a)') 'Rcov(stdev) for instrument: ', trim(ErrorCov%name), ' recond'
       write(6,'(9(1x,es10.3))') sqrt(diag)
       deallocate(diag)
   endif

   initialized_=.true.
end subroutine set_
!EOC

!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  create_ --- creates type to hold observation error covariance
!
! !INTERFACE:
!
subroutine create_ (nch,ErrorCov)
implicit none
! !INPUT PARAMETERS:
integer(i_kind),intent(in) :: nch
! !INPUT/OUTPUT PARAMETERS:
type(ObsErrorCov) :: ErrorCov
! !DESCRIPTION: Allocates space for FORTRAN type hold observation error
!               covariance and required information.
!
! !REVISION HISTORY:
!   2014-04-13  todling  initial code
!
! !REMARKS:
!   language: f90
!   machine:  discover
!
! !AUTHOR:
!   Ricardo Todling  org: gmao      date: 2014-04-13
!
!EOP
!-------------------------------------------------------------------------
!BOC
   allocate(ErrorCov%R(nch,nch))
   allocate(ErrorCov%indxR(nch))
   allocate(ErrorCov%UT(nch,nch),ErrorCov%UTfull(nch,nch))
end subroutine create_
!EOC

!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  destroy_ --- destroy type holding observation error covariance
!
! !INTERFACE:
!
subroutine destroy_ (ErrorCov)
implicit none
! !INPUT/OUTPUT PARAMETERS:
type(ObsErrorCov) :: ErrorCov
! !DESCRIPTION: Deallocates space held for observation error covariance.
!
! !REVISION HISTORY:
!   2014-04-13  todling  initial code
!
! !REMARKS:
!   language: f90
!   machine:  discover
!
! !AUTHOR:
!   Ricardo Todling  org: gmao      date: 2014-04-13
!
!EOP
!-------------------------------------------------------------------------
!BOC
   deallocate(ErrorCov%UT,ErrorCov%UTfull)
   deallocate(ErrorCov%indxR)
   deallocate(ErrorCov%R)
end subroutine destroy_
!EOC


!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  solver_ --- entry-point to the decomposition of cov(R)
!
! !INTERFACE:
!

subroutine solver_(ErrorCov)
implicit none
! !INPUT/OUTPUT PARAMETERS:
type(ObsErrorCov) :: ErrorCov

! !DESCRIPTION: This routine is the entry point to the cholesky factorization
!               and inversion of the obs error covariance.
!
! !REVISION HISTORY:
!   2014-04-13  todling  initial code
!   2015-08-18  W. Gu     Switich the reconditioning method from adding a constant value
!                         to each eigenvalue to adding a constant value in standard deviation to
!                         each diagnoal element.
!   2015-08-18  W. Gu     Bring the modifications of obs errors done in QC to correlated obs errors 
!   2019-04-22  kbathmann Change the options for "method", change from eigendecomposition 
!                         to cholesky factorization, remove reconditioning
!
!
! !REMARKS:
!   language: f90
!   machine:  discover
!
! !AUTHOR:
!   Ricardo Todling  org: gmao      date: 2014-04-13
!
!EOP
!-------------------------------------------------------------------------
!BOC
character(len=*), parameter :: myname_=myname//'*solver_'
real(r_kind) lambda_max,lambda_min,lambda_inc
integer(i_kind) ii,jj,ndim
logical adjspec
real(r_kind),allocatable,dimension(:):: invstd

ndim = size(ErrorCov%R,1)
 
! This extracts the diagonal of R (error variances)
! This is to allow using the estimated error variances, but
! but still pretend the covariance is diagnoal - no correlations.
! This is largely for testing consistency of the implementation.
if ( ErrorCov%method==0) then
   ErrorCov%UTfull = zero
   do ii=1,ndim
      ErrorCov%UTfull(ii,ii)    = ErrorCov%R(ii,ii) 
   enddo
else
! This does the actual full Cholesky factorization of the matrix
   ErrorCov%UTfull=ErrorCov%R
   call decompose_(trim(ErrorCov%name),ErrorCov%UTfull,ndim)
endif
end subroutine solver_
!EOC


!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  decompose_ --- calculates eigen-decomposition of cov(R)
!
! !INTERFACE:
!
subroutine decompose_(instrument,UT,ndim)
! !USES:
use constants, only: tiny_r_kind
  implicit none
! !INPUT PARAMETERS:
  character(len=*),intent(in):: instrument
  integer(i_kind),intent(in) :: ndim
! !INPUT/OUTPUT PARAMETERS:
  real(r_kind),intent(inout) :: UT(:,:) ! on entry: matrix to decompose
                                        ! on exit: inv of U, where R=UU^T

! !DESCRIPTION: This routine makes a LAPACK call to compute the cholesky factorization of R.
!               R is factored as R=UU^T, and then U is inverted to get a
!               non-diagonal square root of R^-1.
!
! !REVISION HISTORY:
!   2014-04-13  todling   initial code
!   2019-04-15  kbathmann change from eigendecomposition to cholesky factorizaiton
!
! !REMARKS:
!   language: f90
!   machine:  discover
!
! !AUTHOR:
!   Ricardo Todling  org: gmao      date: 2014-04-13
!
!EOP
!-------------------------------------------------------------------------
!BOC

  character(len=*),parameter :: myname_=myname//'decompose_'
  integer(i_kind):: infoc,infoi
  if(r_kind==r_single) then ! this trick only works because this uses the f77 lapack interfaces
     call SPOTRF('U',ndim,UT,ndim,infoc)
     call STRTRI('U','N',ndim,UT,ndim,infoi)
  else if(r_kind==r_double) then
     call DPOTRF('U',ndim,UT,ndim,infoc)
     call DTRTRI('U','N',ndim,UT,ndim,infoi)
  else
     call die(myname_,'no corresponding LAPACK call for cholesky inversion')
  endif
  if ((abs(infoc)>0).or.(abs(infoi)>0)) call die(myname_,'trouble with lapack routines')
end subroutine decompose_
!EOC

!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  scale_jac_ ---  scale Jacobian, residuals, and related errors
!
! !INTERFACE:
!
logical function scale_jac_(depart,obvarinv,jacobian, nchanl,&
                            jpch_rad,varinv,wgtjo,iuse,ich,ErrorCov,pred,Rinv,rsqrtinv)
! !USES:
use constants, only: tiny_r_kind
use mpeu_util, only: die
implicit none
! !INPUT PARAMETERS:
integer(i_kind),intent(in) :: nchanl   ! total number of channels in instrument
integer(i_kind),intent(in) :: jpch_rad ! total number of channels in GSI
integer(i_kind),intent(in) :: ich(:)   ! true channel numeber
integer(i_kind),intent(in) :: iuse(0:jpch_rad) ! flag indicating whether channel used or not
real(r_kind),   intent(in) :: varinv(:)    ! inverse of specified ob-error-variance 
! !INPUT/OUTPUT PARAMETERS:
real(r_kind),intent(inout) :: depart(:)    ! observation-minus-guess departure
real(r_kind),intent(inout) :: obvarinv(:)  ! inverse of eval(diag(R)) !KAB delete?
real(r_kind),intent(inout) :: wgtjo(:)     ! weight in Jo-term
real(r_kind),intent(inout) :: jacobian(:,:)! Jacobian matrix
real(r_kind),intent(in)    :: pred(:,:)    ! bias predictors
!real(r_kind),intent(inout) :: cpred(:,:)   ! bias predictor
real(r_kind),intent(inout) :: Rinv(:)
real(r_kind),intent(inout) :: rsqrtinv(:,:)!
type(ObsErrorCov) :: ErrorCov              ! ob error covariance for given instrument

! !DESCRIPTION: This routine is the main entry-point to the outside world. 
!               It redefines the Jacobian matrix so it embeds the inverse of the square root 
!               observation error covariance matrix. Only the sub-matrix related
!               to the active and accepted channels in the given profile is 
!               taken into account.
!
! !REVISION HISTORY:
!   2014-04-13  todling    initial code
!   2014-11-15  W. Gu      bug fix in R-inverse indexation
!   2014-12-19  W. Gu      use the eigenvalue decomposition to form a square root decomposition, and then
!                          apply to correlated R-covariance matrix(R= QD^(1/2)Q^T QD^(1/2)Q^T). 
!   2015-04-01  W. Gu      clean the code
!   2017-07-27  kbathmann  Merge subroutine rsqrtinv into scale_jac, define Rinv to fix
!                          diag_precon for correlated error, and reorder several nested loops
!   2018-03-18  kbathmann  change to cholesky, and move over correlated error code from intrad and stprad
!
! !REMARKS:
!   language: f90
!   machine:  discover
!
! !AUTHOR:
!   Ricardo Todling  org: gmao      date: 2014-04-13
!
!EOP
!-------------------------------------------------------------------------
!BOC

character(len=*),parameter :: myname_=myname//'*scale_jac'
integer(i_kind) :: nch_active,ii,jj,kk,iii,jjj,mm,nn,ncp,ifound,nsigjac,indR,npred
integer(i_kind),allocatable,dimension(:)   :: ircv
integer(i_kind),allocatable,dimension(:)   :: ijac
integer(i_kind),allocatable,dimension(:)   :: IRsubset
integer(i_kind),allocatable,dimension(:)   :: IJsubset
real(r_kind),   allocatable,dimension(:)   :: col
real(r_kind),   allocatable,dimension(:,:) :: row
real(r_kind) :: val
integer(i_kind) :: method
logical subset
scale_jac_=.false.
nch_active=ErrorCov%nch_active
method=ErrorCov%method
if(nch_active<0) return
call timer_ini('scljac')

! get indexes for the internal channels matching those
! used in estimating the observation error covariance
allocate(ircv(nchanl))
allocate(ijac(nchanl))
ircv = -1
ijac = -1
do jj=1,nchanl
   mm=ich(jj)       ! true channel number (has no bearing here except in iuse)
   if (varinv(jj)>tiny_r_kind .and. iuse(mm)>=1) then
      ifound=-1
      do ii=1,nch_active
         if (ErrorCov%nctot>nchanl) then
            indR=ii
         else
            indR=ErrorCov%indxR(ii)
         end if
         if(jj==indR) then
            ifound=ii
            exit
         endif
      enddo
      if(ifound/=-1) then
         ijac(jj)=jj      ! index value applies to the jacobian and departure
         ircv(jj)=ifound  ! index value applies to ErrorCov
      endif
   endif
enddo
ncp=count(ircv>0) ! number of active channels in profile
! following should never happen, but just in case ...
if(ncp==0 .or. ncp>ErrorCov%nch_active) then
   call die(myname_,'serious inconsistency in handling correlated obs')
endif
! Get subset indexes; without QC and other on-the-fly analysis choices these
!                     two indexes would be the same, but because the analysis
!                     remove data here and there, most often there will be fewer
!                     channels being processed for a given profile than the set
!                     of active channels used to get an offline estimate of R.
allocate(IRsubset(ncp)) ! these indexes apply to the matrices/vec in ErrorCov
allocate(IJsubset(ncp)) ! these indexes apply to the Jacobian/departure 
iii=0;jjj=0
do ii=1,nchanl
   if(ircv(ii)>0) then
      iii=iii+1
      IRsubset(iii)=ircv(ii)  ! subset indexes in R presently in use
   endif
   if(ijac(ii)>0) then
      jjj=jjj+1
      IJsubset(iii)=ijac(ii)  ! subset indexes in Jac/dep presently in use
   endif
enddo
if (iii/=ncp) then
   if (iamroot_) then
       write(6,*) myname, ' iii,ncp= ',iii,ncp
   endif
   call die(myname_,' serious dimensions insconsistency (R), aborting')
endif
if (jjj/=ncp) then
   if (iamroot_) then
       write(6,*) myname, ' jjj,ncp= ',jjj,ncp
   endif
   call die(myname_,' serious dimensions insconsistency (J), aborting')
endif

! decompose the sub-matrix - returning the result in the 
!                            structure holding the full covariance
if( method >0 ) then
   if (ncp<ErrorCov%nch_active) then
      subset = decompose_subset_ (IRsubset,ErrorCov)
   else
      ErrorCov%UT=ErrorCov%UTfull
      subset=.true.
   endif
   if(.not.subset) then
      call die(myname_,' failed to decompose correlated R')
   endif
endif

if( method<0 ) then
!  Keep departures and Jacobian unchanged
!  Do as GSI would do otherwise
   do jj=1,ncp
      mm=IJsubset(jj) 
      wgtjo(mm)    = varinv(mm)
      Rinv(jj)=sqrt(wgtjo(mm))
      rsqrtinv(jj,jj)=sqrt(Rinv(jj))
   enddo
elseif (method==0) then
   do jj=1,ncp
      mm=IJsubset(jj)
      val = obvarinv(mm)*varinv(mm)
      wgtjo(mm)    = val/ErrorCov%R(IRsubset(jj),IRsubset(jj))
      rsqrtinv(jj,jj)=sqrt(wgtjo(mm))
      Rinv(jj)=wgtjo(mm)
   enddo
else
   do ii=1,ncp
      mm=IJsubset(ii)
      obvarinv(mm)=obvarinv(mm)*varinv(mm)
   enddo
   nsigjac=size(jacobian,1)
   npred=size(pred,1)
   allocate(row(nsigjac,ncp))
   allocate(col(ncp))
   row=zero 
   rsqrtinv=zero
   col=zero
!   cpred=zero
   Rinv=zero
   if (method==3) then
      do ii=1,ncp
         iii=IRsubset(ii)
         do jj=ii,ncp
            mm=IJsubset(jj)
            jjj=IRsubset(jj)
            ErrorCov%UT(jjj,iii)=ErrorCov%UT(jjj,iii)*obvarinv(mm)
         enddo
      enddo
   endif
   do ii=1,ncp
      iii=IRsubset(ii)
      do jj=ii,ncp
         jjj=IRsubset(jj)
!         do kk=1,npred
!            val=pred(kk,IJsubset(ii))*ErrorCov%UT(iii,jjj)
!            cpred(kk,ii)=cpred(kk,ii)+(val*val)
!         enddo
         Rinv(ii)=Rinv(ii)+ErrorCov%UT(iii,jjj)*ErrorCov%UT(iii,jjj)
      enddo
      do jj=1,ii
         jjj=IRsubset(jj)
         do kk=1,nsigjac
            row(kk,ii)=row(kk,ii)+jacobian(kk,IJsubset(jj))*ErrorCov%UT(jjj,iii)
         enddo
         col(ii)=col(ii)+ErrorCov%UT(jjj,iii)*depart(IJsubset(jj))
      enddo
   enddo
!  Place Jacobian and departure in output arrays
   do jj=1,ncp
      mm=IJsubset(jj)
      depart(mm)=col(jj)
      wgtjo(mm)    = one
      do ii=1,nsigjac
         jacobian(ii,mm)=row(ii,jj)
      end do
      do ii=1,jj
         rsqrtinv(ii,jj)=ErrorCov%UT(IRsubset(ii),IRsubset(jj))
      enddo
   enddo
   deallocate(col)
   deallocate(row)
endif
! clean up
deallocate(IJsubset)
deallocate(IRsubset)
deallocate(ijac,ircv)
scale_jac_=.true.
call timer_fnl('scljac')
end function scale_jac_
!EOC

!BOP
!
! !IROUTINE:  upd_varqc_ ---  replace the obs error prescribed in satinfo for instrument accounted for inter-channel covariance.
!
! !INTERFACE:
!
subroutine upd_varqc_(jpch_rad,iuse_rad,nusis,varch_sea, &
                      varch_land,varch_ice,varch_snow,varch_mixed)
! !USES:
   use mpeu_util, only: die
   use mpeu_util, only: getindex
implicit none
! !INPUT PARAMETERS:
   integer(i_kind),intent(in) :: jpch_rad
   integer(i_kind),dimension(0:jpch_rad),intent(in) :: iuse_rad
   character(len=*),dimension(jpch_rad),intent(in) :: nusis
! !INPUT/OUTPUT PARAMETERS:
   real(r_kind),dimension(jpch_rad),intent(inout) :: varch_sea,varch_land
   real(r_kind),dimension(jpch_rad),intent(inout) ::varch_ice,varch_snow,varch_mixed
! !DESCRIPTION: This routine will replace the prescribed obs errors in satinfo for instruments we account 
!               for inter-channel covariances.
!
! !REVISION HISTORY:
!   2014-11-26  W. Gu     Initial code
!   2019-02-26  kbathmann Update to use Diag of R in QC, rather than satinfo errors.
!                         This subroutine is used in method 0 and 3 only
!
! !REMARKS:
!   language: f90
!   machine:  discover
!
! !AUTHOR:
!   Wei Gu  org: gmao      date: 2014-11-26
!
!EOP
!-------------------------------------------------------------------------
!BOC

   character(len=*),parameter :: myname_=myname//'*upd_varqc'
   character(len=80) covtype
   integer(i_kind) :: nch_active,ii,jj,iii,jjj,mm,nn,ncp,ifound,jj0,itbl,ntrow
   integer(i_kind),dimension(6) ::nsatype
   integer(i_kind)::nsat,isurf,rr
   integer(i_kind),allocatable,dimension(:)   :: ircv
   integer(i_kind),allocatable,dimension(:)   :: ijac
   integer(i_kind),allocatable,dimension(:)   :: IRsubset
   integer(i_kind),allocatable,dimension(:)   :: IJsubset
   integer(i_kind) iinstr,indR
   integer(i_kind),allocatable,dimension(:) :: ich1  ! true channel number
   integer(i_kind),allocatable,dimension(:,:) :: tblidx
   integer(i_kind) :: nchanl1,jc   ! total number of channels in instrument
   if(.not.allocated(idnames)) then
     return
   endif
   ntrow = size(idnames)
   allocate(ich1(jpch_rad),tblidx(5,ntrow))

   nsatype=0
   do jj0=1,ntrow
      if ((GSI_BundleErrorCov(jj0)%method==3).or. &
         (GSI_BundleErrorCov(jj0)%method==0)) then
         covtype=trim(idnames(jj0))
         iinstr=len_trim(covtype)
         if(covtype(iinstr-3:iinstr)==':sea')then
            nsatype(1)=nsatype(1)+1
            nsatype(6)=nsatype(6)+1
            tblidx(1,nsatype(1))=jj0
         endif
         if(covtype(iinstr-4:iinstr)==':land')then
            nsatype(2)=nsatype(2)+1
            nsatype(6)=nsatype(6)+1
            tblidx(2,nsatype(2))=jj0
         endif
         if(covtype(iinstr-3:iinstr)==':ice')then
            nsatype(3)=nsatype(3)+1
            nsatype(6)=nsatype(6)+1
            tblidx(3,nsatype(3))=jj0
         endif
         if(covtype(iinstr-4:iinstr)==':snow')then
            nsatype(4)=nsatype(4)+1
            nsatype(6)=nsatype(6)+1
            tblidx(4,nsatype(4))=jj0
         endif
         if(covtype(iinstr-5:iinstr)==':mixed')then
            nsatype(5)=nsatype(5)+1
            nsatype(6)=nsatype(6)+1
            tblidx(5,nsatype(5))=jj0
         endif
      endif
   enddo
   if(nsatype(6)==0) return
   do isurf=1,5
      nsat=nsatype(isurf)
      if (nsat>0) then
         do jj0=1,nsat
            itbl=tblidx(isurf,jj0) !a row number
            jc=0
            covtype = ''
            ich1=0
            do ii=1,jpch_rad
               if (isurf==1) then
                  covtype = trim(nusis(ii))//':sea'
               else if (isurf==2) then
                  covtype = trim(nusis(ii))//':land'
               else if (isurf==3) then
                  covtype = trim(nusis(ii))//':ice'
               else if (isurf==4) then
                  covtype = trim(nusis(ii))//':snow'
               else if (isurf==5) then
                  covtype = trim(nusis(ii))//':mixed'
               end if
               if(trim(idnames(itbl))==trim(covtype)) then
                  jc=jc+1
                  ich1(jc)=ii
               endif
            enddo
            nchanl1=jc
            if(nchanl1==0) call die(myname_,' improperly set GSI_BundleErrorCov')
            if(.not.amiset_(GSI_BundleErrorCov(itbl)))call die(myname_,' improperly set GSI_BundleErrorCov')
            nch_active=GSI_BundleErrorCov(itbl)%nch_active
            if(nch_active<0) return
! get indexes for the internal channels matching those
! used in estimating the observation error covariance
            allocate(ircv(nchanl1))
            allocate(ijac(nchanl1))
            ircv = -1
            ijac = -1
            do jj=1,nchanl1
               mm=ich1(jj)       ! true channel number (has no bearing here except in iuse)
               if (iuse_rad(mm)>=1) then
                  ifound=-1
                  do ii=1,nch_active
                     if (GSI_BundleErrorCov(itbl)%nctot>nchanl1) then
                        indR=ii
                     else
                        indR=GSI_BundleErrorCov(itbl)%indxR(ii)
                     end if 
                     if(jj==indR) then
                        ifound=ii       
                        exit
                     endif
                  enddo
                  if(ifound/=-1) then
                     ijac(jj)=jj      ! index value in 1 to nchanl
                     ircv(jj)=ifound  ! index value in 1 to nch_active 
                  endif
               endif
            enddo
            ncp=count(ircv>0) ! number of active channels in profile
            if(ncp/=nch_active) then
               call die(myname_,'serious inconsistency in handling correlated obs')
            endif
            allocate(IRsubset(ncp)) ! these indexes apply to the matrices/vec in ErrorCov
            allocate(IJsubset(ncp)) ! these indexes in 1 to nchanl
            iii=0;jjj=0
            do ii=1,nchanl1
               if(ircv(ii)>0) then
                  iii=iii+1
                  IRsubset(iii)=ircv(ii)  ! subset indexes in R presently in use
               endif
               if(ijac(ii)>0) then
                  jjj=jjj+1
                  IJsubset(iii)=ijac(ii)  ! subset indexes in channels presently in use
               endif
            enddo
            if (iii/=ncp) then
               if (iamroot_) then
                  write(6,*) myname, ' iii,ncp= ',iii,ncp
               endif
               call die(myname_,' serious dimensions insconsistency, aborting')
            endif
            if (jjj/=ncp) then
               if (iamroot_) then
                  write(6,*) myname, ' jjj,ncp= ',jjj,ncp
               endif
               call die(myname_,' serious dimensions insconsistency, aborting')
            endif
            do ii=1,ncp
               nn=IJsubset(ii)
               mm=ich1(nn)
               rr=IRsubset(ii)
               if(isurf==1) varch_sea(mm)=sqrt(GSI_BundleErrorCov(itbl)%R(rr,rr))
               if(isurf==2) varch_land(mm)=sqrt(GSI_BundleErrorCov(itbl)%R(rr,rr))
               if(isurf==3) varch_ice(mm)=sqrt(GSI_BundleErrorCov(itbl)%R(rr,rr))
               if(isurf==4) varch_snow(mm)=sqrt(GSI_BundleErrorCov(itbl)%R(rr,rr))
               if(isurf==5) varch_mixed(mm)=sqrt(GSI_BundleErrorCov(itbl)%R(rr,rr))
            enddo
! clean up
            deallocate(IJsubset)
            deallocate(IRsubset)
            deallocate(ijac)
            deallocate(ircv)
         enddo !jj=1,nsat
      endif !nsat >0
   enddo !isurf=1,5
   deallocate(ich1,tblidx)
end subroutine upd_varqc_
!EOC

!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  decompose_subset_ --- extract subset cov(R) and decompose it
!
! !INTERFACE:
!

! !DESCRIPTION: Given an index-set of channels really operative in a given
!               profile, this routine extracts those rows and columns from
!               the offline estimate of cov(R), creating a subset(cov(R))
!               that is eigen-decomposed. The resulting partial eigen-
!               decomposition is stored back in the corresponding 
!               rows and columns of the temporary space in ObErroCov
!               responsible for holding the eigen-values/vectors. 
!
! !REVISION HISTORY:
!   2014-04-13  todling  initial code
!
! !REMARKS:
!   language: f90
!   machine:  discover
!
! !AUTHOR:
!   Ricardo Todling  org: gmao      date: 2014-04-13
!
!EOP
!-------------------------------------------------------------------------
!BOC
logical function decompose_subset_ (Isubset,ErrorCov)
implicit none
! in this approach, we take only the rows and columns of R related 
! to the channels used, and eigendecompose them ... instead of 
! eigendecomposing once, which I think ends up leading to the wrong
! mixt of eigenvalues and eigenvectors.
integer(i_kind),intent(in) :: Isubset(:)
type(ObsErrorCov) :: ErrorCov

character(len=*), parameter :: myname_=myname//'*subset_'
real(r_kind),allocatable,dimension(:,:) :: UT
integer(i_kind) ii,jj,ncp

decompose_subset_=.false. 
ncp=size(Isubset) ! number of channels actually used in this profile
allocate(UT(ncp,ncp))

! extract subcomponent of R
do jj=1,ncp
   do ii=1,ncp
      UT(ii,jj) = ErrorCov%R(Isubset(ii),Isubset(jj))
   enddo
enddo
! decompose subset matrix
call decompose_(ErrorCov%instrument,UT,ncp)
! copy decomposition onto ErrorCov
do jj=1,ncp
   do ii=1,jj
      ErrorCov%UT(Isubset(ii),Isubset(jj)) = UT(ii,jj)
   enddo
enddo
! clean up
deallocate(UT)

decompose_subset_=.true.
end function decompose_subset_
!EOC

!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  amIset_ --- checks whether a particular cov(R) has been set or not
!
! !INTERFACE:
!

logical function amIset_ (ErrorCov)
implicit none
! !INPUT/OUTPUT PARAMETERS:
type(ObsErrorCov) :: ErrorCov

! !DESCRIPTION: This routine returns the status of a particular instance of 
!               the FORTRAN typing holding the observation error covariance.
!
! !REVISION HISTORY:
!   2014-04-13  todling  initial code
!
! !REMARKS:
!   language: f90
!   machine:  discover
!
! !AUTHOR:
!   Ricardo Todling  org: gmao      date: 2014-04-13
!
!EOP
!-------------------------------------------------------------------------
!BOC
logical failed
failed=.false.
amIset_=.false.
if(ErrorCov%nch_active<0) failed=.true.
if(.not.associated(ErrorCov%indxR)) failed=.true.
if(.not.associated(ErrorCov%R)) failed=.true.
if(.not.associated(ErrorCov%UT)) failed=.true.
if(.not.failed) amIset_=.true.
end function amIset_
!EOC

!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  fnl_ --- destroy all instances of cov(R)
!
! !INTERFACE:
!
subroutine fnl_
implicit none

! !DESCRIPTION: Deallocates space held for observation error covariance.
!
! !REVISION HISTORY:
!   2014-04-13  todling  initial code
!
! !REMARKS:
!   language: f90
!   machine:  discover
!
! !AUTHOR:
!   Ricardo Todling  org: gmao      date: 2014-04-13
!
!EOP
!-------------------------------------------------------------------------
!BOC
integer(i_kind) ii,ndim
if(.not.initialized_) return
ndim=size(GSI_BundleErrorCov)
do ii=1,ndim
   call destroy_(GSI_BundleErrorCov(ii))
enddo
deallocate(GSI_BundleErrorCov)
if(allocated(idnames)) deallocate(idnames)
if(allocated(instruments)) deallocate(instruments)
end subroutine fnl_
!EOC

end module correlated_obsmod
