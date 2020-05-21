module bicglanczos
!$$$ module documentation block
!           .      .    .                                       .
! module:   bicglanczos
!   prgmmr: tremolet
!
! abstract: Contains variables and routines for preconditioned
!           Lanczos minimizer based on Bi-CG
!
! program history log:
!   2009-08-10  tremolet
!   2010-10-01  el akkraoui - revisit original implementation (still w/o precond)
!   2011-04-19  el akkraoui - add preconditioning and orthogonalization
!   2011-07-04  todling - determine precision based on kinds
!   2012-07-11  todling - add hyb-ens capability following pcgsoi changes
!   2013-01-23  parrish - in subroutine pcgprecond, change variable xcvx from
!                          intent(in) to intent(inout) (flagged by WCOSS intel debug compiler)
!   2013-11-17  todling - implement convergence criterium (based on tolerance)
!   2015-12-08  el akkraoui - add precond calls for new preconditioning of predictors
!   2016-03-25  todling - beta-mult param now within cov (following Dave Parrish corrections)
!   2016-05-13  parrish - remove call to beta12mult -- replaced by sqrt_beta_s_mult in
!                          bkerror, and sqrt_beta_e_mult inside bkerror_a_en.
!   2017-06-27  todling - knob to bypass calc when gradient is tiny(zero)
!
! Subroutines Included:
!   save_pcgprecond - Save eigenvectors for constructing the next preconditioner
!   setup_pcgprecond - Reads preconditioning
!   pcglanczos       - Actual Lanczos account for left and right eigen-problems
!   pcgprecond       - Apply preconditioning
!
! Variable Definitions:
!   lmpcgl  : .T. ====> precondition conjugate-gradient minimization
!   r_max_cnum_pc : Maximum allowed condition number for the preconditioner
!   npcvecs : number of vectors which make up the preconditioner
!
!   yvcglpc: eigenvectors (from an earlier minimization)
!            that are used to construct the preconditioner.
!   rcglpc : eigenvalues (from an earlier minimization)
!            that are used to construct the preconditioner.
!   nvcglpc: the number of eigenpairs used to form the preconditioner.
!
!   yvcglev: eigenvectors for the current minimization.
!   rcglev : eigenvalues for the current minimization.
!   nvcglev: the number of eigenpairs for the current minimization.
!
!   yvcglwk: work array of eigenvectors
!   yucglwk: work array of eigenvectors
!
! attributes:
!   language: f90
!   machine:
!   
! ------------------------------------------------------------------------------

!=============================================================
use kinds    , only : r_kind,i_kind,r_quad,r_single,r_double
use constants, only : zero, one, half,two, zero_quad,tiny_r_kind
use timermod , only : timer_ini, timer_fnl
use lanczos  , only : save_precond
use gsi_4dvar, only : iorthomax
use control_vectors, only: control_vector
use control_vectors, only: allocate_cv,deallocate_cv,inquire_cv
use control_vectors, only: read_cv,write_cv
use control_vectors, only: dot_product,assignment(=)
use gsi_bundlemod, only: gsi_bundle
use gsi_bundlemod, only: assignment(=)
use mpimod  ,  only : mpi_comm_world
use mpimod,    only: mype
use jfunc   ,  only : iter, jiter, diag_precon,step_start
use gsi_4dvar, only : nwrvecs,l4dvar,lanczosave
use gsi_4dvar, only : nsubwin, nobs_bins
use hybrid_ensemble_parameters,only : l_hyb_ens,aniso_a_en
use hybrid_ensemble_isotropic, only: bkerror_a_en

implicit none
private
save
public pcglanczos, setup_pcglanczos, save_pcgprecond, setup_pcgprecond,& 
       pcgprecond, lmpcgl

!=============================================================
logical      :: ltcost_ = .false.
logical      :: lmpcgl  = .false.
logical      :: allocated_work_vectors=.false.
real(r_kind) :: r_max_cnum_pc = 10.0_r_kind
real(r_kind) :: xmin_ritz = 1.0_r_kind
real(r_kind) :: pkappa = 0.1_r_kind
real(r_kind) :: cg_tol = 0.001_r_kind

integer(i_kind)           :: npcvecs, nvcglpc, nvcglev
real(r_kind), allocatable :: rcglpc(:)
real(r_kind), allocatable :: rcglev(:)

integer(i_kind)           :: nprt,maxiter

type(control_vector), allocatable, dimension(:) :: yvcglpc
type(control_vector), allocatable, dimension(:) :: yvcglev
type(control_vector), allocatable, dimension(:) :: yucglev
type(control_vector), allocatable, dimension(:) :: yvcglwk
type(control_vector), allocatable, dimension(:) :: cglwork
type(control_vector), allocatable, dimension(:) :: cglworkhat

! --------------------------------------
integer(i_kind), parameter :: n_default_real_kind = r_single
integer(i_kind), parameter :: n_double_kind       = r_double

!=============================================================
contains

!=============================================================
subroutine setup_pcglanczos(kpe,kprt,kiter,kiterstart,kmaxit,kwrvecs, &
                         ld4dvar,ldsave,ltcost)
implicit none
integer(i_kind), intent(in) :: kpe,kprt,kiter,kiterstart,kmaxit,kwrvecs
logical        , intent(in) :: ld4dvar,ldsave,ltcost
integer(i_kind)             :: ii,isize

mype=kpe
nprt=kprt
jiter=kiter
maxiter=kmaxit
nwrvecs=kwrvecs
l4dvar=ld4dvar
lanczosave=ldsave
ltcost_=ltcost


if (iorthomax>0) then
   if ( lanczosave ) then 
        isize = maxiter
   else
        isize = iorthomax  
   endif
   allocate(cglwork(isize+1))
   allocate(cglworkhat(isize+1))
   DO ii=1,isize+1
      CALL allocate_cv(cglwork(ii))
      cglwork(ii)=zero
      CALL allocate_cv(cglworkhat(ii))
      cglworkhat(ii)=zero
   enddo
   allocated_work_vectors=.true.
endif 

if (jiter==kiterstart) then
  npcvecs=0
  nvcglpc=0
  nvcglev=0
endif

 if (jiter>1 .and. ldsave )then
    call setup_pcgprecond() !  Calculates the preconditioner for congrad
 endif

call inquire_cv

end subroutine setup_pcglanczos
! -------------------------------------------------------------------

!================================================================
subroutine pcglanczos(xhat,yhat,pcost,gradx,grady,preduc,kmaxit,lsavevecs)

!$$$  subprogram documentation block
!
! abstract: solve inner loop of analysis equation using conjugate
!           gradient preconditioned with B.
!
! program history log:
!   0999-99-99  tremolet - initial code
!   2013-11-20  todling  - revisit to allow code to stop at single iteration
!
!$$$

! Notes:
! o The algorithm can be implemented without the vectors xtry and ytry 
!   to save memory. For now we keep them to make the code more readable.
! o It is possible to reduce further the number of vectors being used
!   by computing dirx=B*diry or xhat=B*yhat at the cost of another
!   product by the B matrix per iteration (likely to be more expensive
!   than the linear algebra to keep both sets of vectors updated).


implicit none

! Declare local variables
character(len=*), parameter :: myname='pcglanczos'
character(len=*), parameter :: subname='truecost'

type(control_vector),intent(inout)      :: xhat,yhat,gradx,grady
real(r_kind)    , intent(out)           :: pcost
real(r_kind)    , intent(inout)         :: preduc
integer(i_kind) , intent(inout)         :: kmaxit
logical         , intent(in)            :: lsavevecs

type(control_vector)      :: grad0,xtry,ytry,gradw,dirx,diry,dirw
real(r_kind), allocatable :: alpha(:),beta(:),delta(:),gam(:)
real(r_kind), allocatable :: zdiag(:),ztoff(:),zwork(:)
real(r_kind), allocatable :: zritz(:),zbnds(:),zevecs(:,:)
real(r_quad)              :: zg0,zgk,zgnew,zfk,zgg,zge
real(r_kind)              :: zeta,zreqrd
integer(i_kind)           :: jj,ilen,ii,info
integer(i_kind)           :: kminit,kmaxevecs,kconv
logical                   :: lsavinc

! --------------------------------------
real             :: z_default_real      ! intentionally not real(r_kind)
integer(i_kind), parameter :: n_default_real_kind = kind(z_default_real)
double precision :: dl_double_precision ! intentionally not real(r_double)
integer(i_kind), parameter :: n_double_kind       = kind(dl_double_precision)


!<<<
integer(i_kind) :: jk,isize,jm
real(r_kind)    :: zdla,zbndlm

type(control_vector) :: gradf
integer              :: iortho
!---------------------------------------------------

#ifdef ibm_sp
logical,allocatable:: select(:)
integer(i_kind):: n,i,j,ldz
integer(i_kind):: iopt,lda,naux
integer(i_kind),allocatable:: indx(:)
real(r_kind),allocatable:: aux(:),w_order(:)
complex(r_kind),allocatable:: w(:),z(:)
#endif

! Initialize timer
call timer_ini('pcglanczos')

kminit =  kmaxit
kmaxevecs = kmaxit
if (kmaxit>maxiter) then
   write(6,*) 'setup_pcglanczos : kmaxit>maxiter', kmaxit,maxiter
   call stop2(138)
end if

if (mype==0) write(6,*) '---- BICG Solver -----'

! Allocate local variables
call allocate_cv(grad0) ! not in PCGSOI (use ydiff instead)
call allocate_cv(xtry)  ! not in PCGSOI
call allocate_cv(ytry)  ! not in PCGSOI
call allocate_cv(gradw)
call allocate_cv(dirx)
call allocate_cv(diry)
if(nprt>=1.and.ltcost_) call allocate_cv(gradf)
if(diag_precon) call allocate_cv(dirw)

!--- 'zeta' is an upper bound on the relative error of the gradient.

zeta  = 1.0e-4_r_kind
zreqrd = preduc

ilen=xhat%lencv


if(diag_precon) dirw=zero

!$omp parallel do
do jj=1,ilen
  grad0%values(jj)=gradx%values(jj)
  dirx%values(jj)=-grady%values(jj)
  diry%values(jj)=-gradx%values(jj)
end do
!$omp end parallel do

if(diag_precon) then
  do jj=1,ilen
     dirw%values(jj)=diry%values(jj)
  end do 
  call precond(diry)
end if

if(lmpcgl) then 
   dirx=zero
   call pcgprecond(gradx,dirx)
   do jj=1,ilen
       dirx%values(jj) = -dirx%values(jj)
   end do
end if

zg0=dot_product(gradx,grady,r_quad)
if(zg0<tiny_r_kind) then ! this is unlikely to occur, expect when nobs=0
   ! clean up and ...
   if(diag_precon) call deallocate_cv(dirw)
   if(nprt>=1.and.ltcost_) call deallocate_cv(gradf)
   call deallocate_cv(diry)
   call deallocate_cv(dirx)
   call deallocate_cv(gradw)
   call deallocate_cv(ytry)  ! not in PCGSOI
   call deallocate_cv(xtry)  ! not in PCGSOI
   call deallocate_cv(grad0) ! not in PCGSOI (use ydiff instead)
   if (mype==0) then
       write(6,999)trim(myname),': zero gradient, likely no observations', jiter,iter,zg0
   endif
   return ! get out of here.
endif

allocate(alpha(kmaxit),beta(kmaxit),delta(0:kmaxit),gam(0:kmaxit))
alpha(:)=zero_quad
beta(:)=zero_quad

zgk=zg0
delta(0)=zg0
zg0=sqrt(zg0)
gam(0)=one/zg0


zgg=dot_product(dirx,dirx,r_quad)
zgg=sqrt(zgg)
if (iorthomax>0) then
   do jj=1,ilen
      cglwork(1)%values(jj)=gradx%values(jj)/zg0
      cglworkhat(1)%values(jj)=grady%values(jj)/zg0
   enddo
endif 

! ------------------------------------------------------------------------------
! Perform CG inner iterations
! ------------------------------------------------------------------------------
inner_iteration: do iter=1,kmaxit
  if (mype==0) write(6,*)trim(myname),': Minimization iteration',iter

! Estimate
!  do jj=1,ilen
!    xtry%values(jj)=xhat%values(jj)+dirx%values(jj)
!    ytry%values(jj)=yhat%values(jj)+diry%values(jj)
!  end do
  xtry=dirx
  ytry=diry

! Apply the Hessian
  lsavinc=.false.
  call jgrad(xtry,ytry,zfk,gradw,lsavinc,nprt,myname)
  pcost=zfk

  
! Get A q_k
  do jj=1,ilen
    gradw%values(jj)=gradw%values(jj)-grad0%values(jj)
  end do

! Calculate stepsize
  delta(iter)=dot_product(dirx,gradw,r_quad)
  alpha(iter)=zgk/delta(iter)

! Update estimates
  do jj=1,ilen
    xhat%values(jj) =xhat%values(jj) +alpha(iter)*dirx%values(jj)
    yhat%values(jj) =yhat%values(jj) +alpha(iter)*diry%values(jj)
    gradx%values(jj)=gradx%values(jj)+alpha(iter)*gradw%values(jj)
  end do

! First re-orthogonalization
  
  if(iorthomax>0) then
    iortho=min(iter,iorthomax)
    do jm=iortho,1,-1
       zdla = dot_product(gradx,cglworkhat(jm))
       do jj=1,ilen
          gradx%values(jj) = gradx%values(jj) - zdla*cglwork(jm)%values(jj)
       end do
    end do
  end if

! Apply B or the preconditioner

  if(lmpcgl) then 
     call pcgprecond(gradx,grady)
  else 
     call bkerror(gradx,grady)
     ! If hybrid ensemble run, then multiply ensemble control variable a_en 
     !                                 by its localization correlation
     if(l_hyb_ens) then
     
       if(aniso_a_en) then
     !   call anbkerror_a_en(gradx,grady)    !  not available yet
         write(6,*)' ANBKERROR_A_EN not written yet, program stops'
         stop
       else
         call bkerror_a_en(gradx,grady)
       end if
 
     end if
  endif

! Add potential additional preconditioner
  if(diag_precon) call precond(grady)


! Second re-orthogonalization  

  if(iorthomax>0) then
     iortho=min(iter,iorthomax)
     do jm=iortho,1,-1
        zdla = dot_product(grady,cglwork(jm))
        do jj=1,ilen
           grady%values(jj) = grady%values(jj) - zdla*cglworkhat(jm)%values(jj)
        end do
     end do
  end if

! Compute beta
  zgnew=dot_product(gradx,grady,r_quad)
  beta(iter)=zgnew/zgk
  zgk=zgnew

! Evaluate cost for printout
  if(nprt>=1.and.ltcost_) then
    call jgrad(xhat,yhat,zfk,gradf,lsavinc,nprt,subname)
    if (mype==0) then
        write(6,999)trim(myname),': grepcost cost=', jiter,iter,zfk
    endif
  endif
 
! Update search direction
  if(diag_precon) then
    do jj=1,ilen
       diry%values(jj)=dirw%values(jj)
    enddo 
  end if 
  do jj=1,ilen
    dirx%values(jj)=-grady%values(jj)+beta(iter)*dirx%values(jj)
    diry%values(jj)=-gradx%values(jj)+beta(iter)*diry%values(jj)
  end do
  if(diag_precon) then
    do jj=1,ilen
       dirw%values(jj)=diry%values(jj)
    end do 
    call precond(diry)
  end if

! Diagnostics
  if(zgk < zero) then 
     if(mype==0) write(6,*) '*** STOP : Breakdown ***'
     call stop2(999)
  end if
  zgg=sqrt(zgk)
  gam(iter)=one/zgg
  if (mype==0) then
    write(6,999)trim(myname),': grepgrad grad,reduction=', &
      jiter,iter,zgg,zgg/zg0
    write(6,999)trim(myname),': grepgrad alpha,beta=', &
      jiter,iter,alpha(iter),beta(iter)
  endif

! Save Lanczos vectors
  if ( iorthomax > 0  ) then 
     if ( (iter <= iorthomax) .OR. ( lsavevecs ) ) then
        do jj=1,ilen
           cglwork(iter+1)%values(jj)=gradx%values(jj)/zgg
           cglworkhat(iter+1)%values(jj)=grady%values(jj)/zgg
        enddo
     endif 
  endif 

  if(abs(zgg/zg0)<CG_TOL) then
     if (mype==0) then
       write(6,999)trim(myname),': CG achieved desired level of convergence'
     endif
     exit
  endif
end do inner_iteration
kconv=min(iter-1,kmaxit)
! ------------------------------------------------------------------------------
! Done CG inner iterations
! ------------------------------------------------------------------------------
if(kconv>0 .and. lsavevecs) then

! ------------------------------------------------------------------------------
! Lanczos diagnostics
! ------------------------------------------------------------------------------
   allocate(zdiag(0:kconv),ztoff(kconv),zwork(2*kconv))
   allocate(zritz(0:kconv),zbnds(0:kconv))
   allocate(zevecs(kconv+1,kconv+1))

! Build tri-diagonal matrix
   info=0
   zdiag(0)=delta(0)
   do ii=1,kconv
     zdiag(ii)=delta(ii)+beta(ii)*beta(ii)*delta(ii-1)
   enddo
   do ii=0,kconv
     zdiag(ii)=zdiag(ii)*gam(ii)*gam(ii)
   enddo
   do ii=1,kconv
     ztoff(ii)=-beta(ii)*gam(ii)*gam(ii-1)*delta(ii-1)
   enddo

   zge=sqrt(dot_product(cglwork(kconv+1),cglwork(kconv+1)))
   zge=zge* ztoff(kconv)

#ifdef ibm_sp

!  Use ESSL
   iopt=1
   n=kconv+1
   lda=n
   ldz=kconv+1
   naux=2*n
   allocate(select(n),indx(n),w(n),z(n),aux(naux),w_order(n))

!  Additional initializations
   select=.false.    ! select not used for iopt=1
   w=zero
   z=zero
   aux=zero

!  Call ESSL routines
   if (r_kind == n_default_real_kind) then
      call sgeev(iopt, zdiag, lda, w, z, ldz, select, n, aux, naux)
      do i=1,n
         w_order(i)=real(w(i),r_kind)
      end do
      call ssortx(w_order,1,n,indx)  ! sort eigenvalues into ascending order
   elseif (r_kind == n_double_kind) then
      call dgeev(iopt, zdiag, lda, w, z, ldz, select, n, aux, naux)
      do i=1,n
         w_order(i)=real(w(i),r_kind)
      end do
      call dsortx(w_order,1,n,indx)  ! sort eigenvalues into ascending order
   else
      write(6,*)'STEQR: r_kind is neither default real nor double precision'
      call stop2(319)
   endif

!  Load ESSL eigenvalues and eigenvectors into output arrays
   do j=1,n
      zdiag(j)=w_order(j)          ! eigenvalues
      jj=indx(j)
      ztoff(j)=real(z(j),r_kind) ! eigenvectors
   end do

!  Deallocate work arrays
   deallocate(select,indx,w,z,aux,w_order)

#else

!   Use LAPACK
! Get eigen-pairs of tri-diagonal matrix
   if(iter /= 1) then
      if (r_kind==n_default_real_kind) then
         call ssteqr('I',kconv+1,zdiag,ztoff,zevecs,kconv+1,zwork,info)
      elseif (r_kind==n_double_kind) then
         call dsteqr('I',kconv+1,zdiag,ztoff,zevecs,kconv+1,zwork,info)
      else
         write(6,*)trim(myname),': r_kind is neither default real nor double precision'
         call stop2(319)
      endif
   else 
      zevecs(1,1) =one
   endif
#endif

   if (info/=0) then
     write(6,*)trim(myname),': SSTEQR/DSTEQR returned info=',info
     call stop2(320)
   endif

! Error bounds
   zritz(:)=zdiag(:)
   zbndlm = zeta*zritz(kconv)

   if (mype==0) write(6,*)trim(myname),': ritz values are:  ',zritz(:)
   zbnds(:)=abs(zge*zevecs(kconv+1,:))
   if (mype==0) write(6,*)trim(myname),': error bounds are: ',zbnds(:)
   zbnds(:)=zbnds(:)/zritz(:)
   if (mype==0) write(6,*)trim(myname),': relative errors:  ',zbnds(:)

! Compute the eigenvectors

!if (lsavevecs) then

   nvcglev = 0
   do jk=iter,1,-1
      if (zbnds(jk) <= pkappa .and. zritz(jk) > xmin_ritz) then
         nvcglev=nvcglev+1
      endif
   enddo
   if (mype==0) write(6,*) &
          'Number of eigenpairs converged to requested accuracy NVCGLEV=',nvcglev

   nvcglev= min(nwrvecs,nvcglev) 
   if(mype==0) write(6,*) 'Number of eigenvectors to be calculated is', nvcglev

   allocate(rcglev(nvcglev))
   allocate(yvcglev(nvcglev))
   allocate(yucglev(nvcglev))
   do ii=1,nvcglev
      call allocate_cv(yvcglev(ii))
      call allocate_cv(yucglev(ii))
   enddo

   ii=0
   do jk=iter,1,-1
      if (zbnds(jk) <= pkappa .and. zritz(jk) > xmin_ritz .and. ii < nvcglev) then
         ii = ii+1
         rcglev(ii) = zritz(jk)
         yvcglev(ii) = zero
         yucglev(ii) = zero
         xtry=zero
         ytry=zero
         isize=size(xtry%values)
 
         do jm=1,iter
            do jj=1,isize
               xtry%values(jj)=xtry%values(jj)+cglwork(jm)%values(jj)*zevecs(jm,jk)
            enddo
         enddo
         do jm=1,iter
            do jj=1,isize
               ytry%values(jj)= ytry%values(jj)+cglworkhat(jm)%values(jj)*zevecs(jm,jk)
            enddo
         enddo
         zdla=dot_product(xtry,ytry)
         do jj=1,isize
            xtry%values(jj)=xtry%values(jj)/sqrt(zdla)
            ytry%values(jj)=ytry%values(jj)/sqrt(zdla)
         end do
      
         do jj=1,isize
            yvcglev(ii)%values(jj) = xtry%values(jj)
            yucglev(ii)%values(jj) = ytry%values(jj)
         end do

         do jm=1,ii-1
            zdla=dot_product (yucglev(jm),yvcglev(ii))
            do jj=1,isize
               yvcglev(ii)%values(jj) = yvcglev(ii)%values(jj) - zdla*yvcglev(jm)%values(jj)
            enddo
         enddo
         do jm=1,ii-1
            zdla=dot_product (yvcglev(jm),yucglev(ii))
            do jj=1,isize
               yucglev(ii)%values(jj) = yucglev(ii)%values(jj) - zdla*yucglev(jm)%values(jj)
            enddo
         enddo
         zdla=dot_product (yvcglev(ii),yucglev(ii))
         yvcglev(ii)%values = yvcglev(ii)%values / sqrt(zdla)
         yucglev(ii)%values = yucglev(ii)%values / sqrt(zdla)
      endif
   enddo
   
   ii=0
   do jk=iter,1,-1
      if((zbnds(jk) < pkappa).and.(zritz(jk) > xmin_ritz) .and. (ii < nvcglev)) then 
         ii = ii+1
         yvcglev(ii) = zero 
         yvcglev(ii)%values = yucglev(ii)%values  ! Since we only need to save ytry, we keep in YVCGLEV
                                                  ! to avoid too much change in the preconditiong code
      end if
   end do

   if (mype==0.and.nvcglev>0) then
      write(6,'(/)')
      write(6,*)'Calculated eigenvectors for the following eigenvalues:'
      write(6,*)'RCGLEV=',rcglev(1:nvcglev)
      write(6,'(/)')
   endif
   
   do jj=1,nvcglev
      call deallocate_cv(yucglev(jj))
   enddo
   deallocate(yucglev)
!end if


   deallocate(zevecs)
   deallocate(zritz,zbnds)
   deallocate(zdiag,ztoff,zwork)
endif ! kconv>0
! ------------------------------------------------------------------------------
! Release memory

deallocate(alpha,beta,delta,gam)
call deallocate_cv(grad0)
call deallocate_cv(xtry)
call deallocate_cv(ytry)
call deallocate_cv(gradw)
call deallocate_cv(dirx)
call deallocate_cv(diry)
if(nprt>=1.and.ltcost_) call deallocate_cv(gradf)
if(diag_precon) call deallocate_cv(dirw)

call inquire_cv

! Finalize timer
call timer_fnl('pcglanczos')

888 format(2A,3(1X,ES25.18))
999 format(2A,2(1X,I3),3(1X,ES25.18))

return
end subroutine pcglanczos

!=============================================================
!==========================================================
! ------------------------------------------------------------------------------
!   SAVE_PRECOND - Save eigenvectors from CONGRAD for next minimization
! ------------------------------------------------------------------------------
subroutine save_pcgprecond(ldsave)

implicit none

logical, intent(in)       :: ldsave

integer(i_kind)           :: ii,jj, iunit, ivecs, isize
real(r_kind)              :: zz
character(len=13)         :: clfile

if (ldsave) then

!--- read eigenvalues of the preconditioner

  npcvecs = nvcglev+nvcglpc
  if (mype==0) write(6,*)'save_pcgprecond: NVCGLEV,NVCGLPC,NPCVECS=', &
                                        nvcglev,nvcglpc,npcvecs

  allocate(yvcglwk(npcvecs))
  ii=0

!--- copy preconditioner vectors to work file

  if (mype==0.and.nvcglpc>0) write(6,*)'save_pcgprecond: RCGLPC=',rcglpc
  do jj=1,nvcglpc
    ii=ii+1
    zz=sqrt(rcglpc(jj)-one)
    call allocate_cv(yvcglwk(ii))
    yvcglwk(ii)%values = zz * yvcglpc(jj)%values
    call deallocate_cv(yvcglpc(jj))
  enddo
  if (allocated(yvcglpc)) deallocate(yvcglpc)
  if (allocated( rcglpc)) deallocate( rcglpc)
  nvcglpc=0

!--- copy and transform eigenvectors of preconditioned Hessian
 
  if (mype==0.and.nvcglev>0) write(6,*)'save_pcgprecond: RCGLEV=',rcglev
  do jj=1,nvcglev
    ii=ii+1
  !  zz=sqrt(one - one/rcglev(jj))
    zz = min(10._r_kind,rcglev(jj))
    zz = sqrt(one - one/zz)
    call allocate_cv(yvcglwk(ii))
    yvcglwk(ii)%values = zz * yvcglev(jj)%values
    call deallocate_cv(yvcglev(jj))
  enddo
 
  if (allocated(yvcglev)) deallocate(yvcglev)
  if (allocated(yucglev)) deallocate(yucglev)
  if (allocated( rcglev)) deallocate( rcglev)
  nvcglev=0
  
!--- Save the eigenvectors

!+  if (l4dvar) then
  ivecs=min(npcvecs,nwrvecs)
  do jj=1,ivecs
    clfile='evec.XXX.YYYY'
    write(clfile(6:8) ,'(I3.3)') jiter
    write(clfile(10:13),'(I4.4)') jj
    call write_cv(yvcglwk(jj),clfile)
  enddo

  if (mype==0) then
    iunit=78
    clfile='numpcvecs.XXX'
    write(clfile(11:13),'(I3.3)') jiter
    open(iunit,file=clfile)
    write(iunit,*)ivecs
    close(iunit)
  endif

  do ii=1,npcvecs
    call deallocate_cv(yvcglwk(ii))
  enddo
  deallocate(yvcglwk)
!+  else
!+   ! do ii=nwrvecs+1,npcvecs   ! nwrvecs here is -1, the do loop would with 0 ==> allocation pb.  
!+     do ii=1,npcvecs 
!+      CALL deallocate_cv(yvcglwk(ii))
!+    enddo
   !  npcvecs=min(npcvecs,nwrvecs)
!+  endif

endif

if (allocated_work_vectors) then
   if ( lanczosave ) then
        isize = maxiter
   else
        isize = iorthomax
   endif
   do ii=1,isize+1
     call deallocate_cv(cglwork(ii))
   enddo
   deallocate(cglwork)
   do ii=1,isize+1
     call deallocate_cv(cglworkhat(ii))
   enddo
   deallocate(cglworkhat)
endif

return
end subroutine save_pcgprecond
!==========================================================
! ------------------------------------------------------------------------------
!   SETUP_PCGPRECOND - Calculates the preconditioner for congrad
! ------------------------------------------------------------------------------
subroutine setup_pcgprecond()

implicit none

integer(i_kind)                :: jj,jk,ii,iunit
character(len=13)              :: clfile

!--- read vectors, apply change of variable and copy to work file


!+ if (l4dvar) then
iunit=78
clfile='numpcvecs.XXX'
write(clfile(11:13),'(I3.3)') jiter-1
open(iunit,file=clfile)
read(iunit,*)npcvecs
close(iunit)
if (npcvecs<1) then
  write(6,*)'setup_pcgprecond: no vectors for preconditioner',npcvecs
  call stop2(140)
end if

 

allocate(yvcglwk(npcvecs))
do ii=1,npcvecs
  call allocate_cv(yvcglwk(ii))
enddo

  

do jj=1,npcvecs
  clfile='evec.XXX.YYYY'
  write(clfile(6:8) ,'(I3.3)') jiter-1
  write(clfile(10:13),'(I4.4)') jj
  call read_cv(yvcglwk(jj),clfile)
enddo
!+ endif
  

if (allocated(yvcglpc)) then
   do jj=1,nvcglpc
      call deallocate_cv(yvcglpc(jj))
   enddo
   deallocate(yvcglpc)
   nvcglpc=0
endif
if (allocated(rcglpc)) deallocate(rcglpc)
 
nvcglpc = npcvecs
  
allocate (yvcglpc(nvcglpc))
do jj=1,nvcglpc
  call allocate_cv(yvcglpc(jj))
enddo
 
do jj=1,nvcglpc
   yvcglpc(jj) = zero
   do jk=1,yvcglpc(jj)%lencv
      yvcglpc(jj)%values(jk) = yvcglwk(jj)%values(jk)
   enddo
enddo
lmpcgl = .true.


do jj=1,npcvecs
   call deallocate_cv(yvcglwk(jj))
enddo
deallocate(yvcglwk)

npcvecs = 0


return
end subroutine setup_pcgprecond
!===============================================================================

! ------------------------------------------------------------------------------
!   PRECOND - Preconditioner for minimization
! ------------------------------------------------------------------------------
subroutine pcgprecond(xcvx,ycvx)

implicit none

type(control_vector) , intent(inout) :: xcvx
type(control_vector) , intent(inout) :: ycvx

real(r_kind)        :: zdp(nvcglpc)
integer(i_kind)     :: jk, ji
 
ycvx=zero
do jk=1,nvcglpc
   zdp(jk) = 0._r_kind
enddo

!Apply B
call bkerror(xcvx,ycvx)

! If hybrid ensemble run, then multiply ensemble control variable a_en 
!                                 by its localization correlation
if(l_hyb_ens) then

  if(aniso_a_en) then
!   call anbkerror_a_en(xcvx,ycvx)    !  not available yet
    write(6,*)' ANBKERROR_A_EN not written yet, program stops'
    call stop2(999)
  else
    call bkerror_a_en(xcvx,ycvx)
  end if

end if


do jk=1,nvcglpc
    zdp(jk) = dot_product(xcvx,yvcglpc(jk))
enddo

do jk=1,nvcglpc
   do ji=1,xcvx%lencv
      ycvx%values(ji) = ycvx%values(ji) - yvcglpc(jk)%values(ji) * zdp(jk)
   enddo
enddo

return
end subroutine pcgprecond
!=========================================================
!===========
end module bicglanczos
