module setupcldch_mod
use abstract_setup_mod
  type, extends(abstract_setup_class) :: setupcldch_class
  contains
    procedure, pass(this) :: setup => setupcldch
    procedure, pass(this) :: init_vars_derived => init_vars_cldch
  end type setupcldch_class
contains
  subroutine setupcldch(this,lunin,mype,bwork,awork,nele,nobs,is,conv_diagsave)
  !$$$  subprogram documentation block
  !                .      .    .                                       .
  ! subprogram:    setupcldch    compute rhs for conventional surface cldch
  !   prgmmr: derber           org: np23                date: 2004-07-20
  !
  ! abstract: For sea surface temperature observations
  !              a) reads obs assigned to given mpi task (geographic region),
  !              b) simulates obs from guess,
  !              c) apply some quality control to obs,
  !              d) load weight and innovation arrays used in minimization
  !              e) collects statistics for runtime diagnostic output
  !              f) writes additional diagnostic information to output file
  !
  ! program history log:
  !   2015-07-10  pondeca
  !
  !   input argument list:
  !     lunin    - unit from which to read observations
  !     mype     - mpi task id
  !     nele     - number of data elements per observation
  !     nobs     - number of observations
  !
  !   output argument list:
  !     bwork    - array containing information about obs-ges statistics
  !     awork    - array containing information for data counts and gross checks
  !
  ! attributes:
  !   language: f90
  !   machine:  ibm RS/6000 SP
  !
  !$$$
    use mpeu_util, only: die,perr
    use kinds, only: r_kind,r_single,r_double,i_kind
  
    use guess_grids, only: hrdifsig,ges_lnprsl,nfldsig,ntguessig
    use obsmod, only: cldchhead,cldchtail,rmiss_single,i_cldch_ob_type,obsdiags,&
                      lobsdiagsave,nobskeep,lobsdiag_allocated,time_offset,bmiss
    use obsmod, only: cldch_ob_type
    use obsmod, only: obs_diag,luse_obsdiag
    use gsi_4dvar, only: nobs_bins,hr_obsbin
    use oneobmod, only: magoberr,maginnov,oneobtest
    use gridmod, only: nlat,nlon,istart,jstart,lon1,nsig
    use gridmod, only: get_ij
    use constants, only: zero,tiny_r_kind,one,half,one_tenth,wgtlim, &
              two,cg_term,huge_single
    use jfunc, only: jiter,last,miter
    use qcmod, only: dfact,dfact1,npres_print
    use convinfo, only: nconvtype,cermin,cermax,cgross,cvar_b,cvar_pg,ictype
    use convinfo, only: icsubtype
    use m_dtime, only: dtime_setup, dtime_check, dtime_show
    use gsi_bundlemod, only : gsi_bundlegetpointer
    use gsi_metguess_mod, only : gsi_metguess_get,gsi_metguess_bundle
    implicit none
  
  ! Declare passed variables
      class(setupcldch_class)                              , intent(inout) :: this
    logical                                          ,intent(in   ) :: conv_diagsave
    integer(i_kind)                                  ,intent(in   ) :: lunin,mype,nele,nobs
    real(r_kind),dimension(100+7*nsig)               ,intent(inout) :: awork
    real(r_kind),dimension(npres_print,nconvtype,5,3),intent(inout) :: bwork
    integer(i_kind)                                  ,intent(in   ) :: is	! ndat index
  
  ! Declare external calls for code analysis
    external:: tintrp2a11
    external:: stop2
  
  ! Declare local parameters
    real(r_kind),parameter:: r0_1_bmiss=one_tenth*bmiss
    character(len=*),parameter:: myname='setupcldch'
  
  ! Declare local variables
    
    real(r_double) rstation_id
  
    real(r_kind) cldchges,dlat,dlon,ddiff,dtime,error
    real(r_kind) scale,val2,ratio,ressw2,ress,residual
    real(r_kind) obserrlm,obserror,val,valqc
    real(r_kind) term,rwgt
    real(r_kind) cg_cldch,wgross,wnotgross,wgt,arg,exp_arg,rat_err2
    real(r_kind) ratio_errors,tfact
    real(r_kind) errinv_input,errinv_adjst,errinv_final
    real(r_kind) err_input,err_adjst,err_final
    real(r_kind),dimension(nobs):: dup
    real(r_kind),dimension(nele,nobs):: data
    real(r_single),allocatable,dimension(:,:)::rdiagbuf
  
  
    integer(i_kind) ier,ilon,ilat,icldch,id,itime,ikx,imaxerr,iqc
    integer(i_kind) iuse,ilate,ilone,istnelv,iobshgt,izz,iprvd,isprvd
    integer(i_kind) i,nchar,nreal,k,ii,ikxx,nn,ibin,ioff,ioff0,jj
    integer(i_kind) l,mm1
    integer(i_kind) istat
    integer(i_kind) idomsfc
    
    logical,dimension(nobs):: luse,muse
    logical proceed
  
    character(8) station_id
    character(8),allocatable,dimension(:):: cdiagbuf
    character(8),allocatable,dimension(:):: cprvstg,csprvstg
    character(8) c_prvstg,c_sprvstg
    real(r_double) r_prvstg,r_sprvstg
  
    logical:: in_curbin, in_anybin
    integer(i_kind),dimension(nobs_bins) :: n_alloc
    integer(i_kind),dimension(nobs_bins) :: m_alloc
    type(cldch_ob_type),pointer:: my_head
    type(obs_diag),pointer:: my_diag
  
  
    equivalence(rstation_id,station_id)
    equivalence(r_prvstg,c_prvstg)
    equivalence(r_sprvstg,c_sprvstg)
    
    this%numvars = 3
    allocate(this%varnames(this%numvars))
    this%varnames(1:this%numvars) = (/ 'var::ps', 'var::z', 'var::cldch' /)
  ! Check to see if required guess fields are available
    call this%check_vars_(proceed)
    if(.not.proceed) return  ! not all vars available, simply return
  
  ! If require guess vars available, extract from bundle ...
    call this%init_vars_derived
  
    n_alloc(:)=0
    m_alloc(:)=0
  !*********************************************************************************
  ! Read and reformat observations in work arrays.
    read(lunin)data,luse
  !  index information for data array (see reading routine)
    ier=1       ! index of obs error
    ilon=2      ! index of grid relative obs location (x)
    ilat=3      ! index of grid relative obs location (y)
    icldch=4    ! index of cldch observation - background
    id=5        ! index of station id
    itime=6     ! index of observation time in data array
    ikxx=7      ! index of ob type
    imaxerr=8   ! index of cldch max error
    iqc=9       ! index of quality mark
    iuse=10     ! index of use parameter
    idomsfc=11  ! index of dominant surface type
    ilone=12    ! index of longitude (degrees)
    ilate=13    ! index of latitude (degrees)
    istnelv=14  ! index of station elevation (m)
    iobshgt=15  ! index of observation height (m)
    izz=16      ! index of surface height
    iprvd=17    ! index of provider
    isprvd=18   ! index of subprovider
  
    do i=1,nobs
       muse(i)=nint(data(iuse,i)) <= jiter
    end do
  
  ! Check for missing data  !need obs value and error
    do i=1,nobs
      if (data(icldch,i) > r0_1_bmiss)  then
         muse(i)=.false.
         data(icldch,i)=rmiss_single   ! for diag output
         data(iobshgt,i)=rmiss_single  ! for diag output
      end if
  
  !   set any observations larger than 20000.0 to be 20000.0
      if (data(icldch,i) > 20000.0_r_kind) data(icldch,i)=20000.0_r_kind !REVISE VALUE / MPondeca , 17Jul2015
    end do
  
    dup=one
    do k=1,nobs
       do l=k+1,nobs
          if(data(ilat,k) == data(ilat,l) .and.  &
             data(ilon,k) == data(ilon,l) .and.  &
             data(ier,k) < 10000.0_r_kind .and. data(ier,l) < 10000.0_r_kind .and. &
             muse(k) .and. muse(l))then
  
             tfact=min(one,abs(data(itime,k)-data(itime,l))/dfact1)
             dup(k)=dup(k)+one-tfact*tfact*(one-dfact)
             dup(l)=dup(l)+one-tfact*tfact*(one-dfact)
          end if
       end do
    end do
  
  
  ! If requested, save select data for output to diagnostic file
    if(conv_diagsave)then
       ii=0
       nchar=1
       ioff0=22
       nreal=ioff0
       if (lobsdiagsave) nreal=nreal+4*miter+1
       allocate(cdiagbuf(nobs),rdiagbuf(nreal,nobs))
       allocate(cprvstg(nobs),csprvstg(nobs))
    end if
  
    mm1=mype+1
    scale=one
  
    call dtime_setup()
    do i=1,nobs
      dtime=data(itime,i)
      call dtime_check(dtime, in_curbin, in_anybin)
      if(.not.in_anybin) cycle
  
      if(in_curbin) then
         dlat=data(ilat,i)
         dlon=data(ilon,i)
  
         ikx  = nint(data(ikxx,i))
         error=data(ier,i)
       endif
  
  !    Link observation to appropriate observation bin
       if (nobs_bins>1) then
          ibin = NINT( dtime/hr_obsbin ) + 1
       else
          ibin = 1
       endif
       IF (ibin<1.OR.ibin>nobs_bins) write(6,*)mype,'Error nobs_bins,ibin= ',nobs_bins,ibin
  
  !    Link obs to diagnostics structure
       if(luse_obsdiag)then
          if (.not.lobsdiag_allocated) then
             if (.not.associated(obsdiags(i_cldch_ob_type,ibin)%head)) then
                allocate(obsdiags(i_cldch_ob_type,ibin)%head,stat=istat)
                if (istat/=0) then
                   write(6,*)'setupcldch: failure to allocate obsdiags',istat
                   call stop2(295)
                end if
                obsdiags(i_cldch_ob_type,ibin)%tail => obsdiags(i_cldch_ob_type,ibin)%head
             else
                allocate(obsdiags(i_cldch_ob_type,ibin)%tail%next,stat=istat)
                if (istat/=0) then
                   write(6,*)'setupcldch: failure to allocate obsdiags',istat
                   call stop2(295)
                end if
                obsdiags(i_cldch_ob_type,ibin)%tail => obsdiags(i_cldch_ob_type,ibin)%tail%next
             end if
             allocate(obsdiags(i_cldch_ob_type,ibin)%tail%muse(miter+1))
             allocate(obsdiags(i_cldch_ob_type,ibin)%tail%nldepart(miter+1))
             allocate(obsdiags(i_cldch_ob_type,ibin)%tail%tldepart(miter))
             allocate(obsdiags(i_cldch_ob_type,ibin)%tail%obssen(miter))
             obsdiags(i_cldch_ob_type,ibin)%tail%indxglb=i
             obsdiags(i_cldch_ob_type,ibin)%tail%nchnperobs=-99999
             obsdiags(i_cldch_ob_type,ibin)%tail%luse=.false.
             obsdiags(i_cldch_ob_type,ibin)%tail%muse(:)=.false.
             obsdiags(i_cldch_ob_type,ibin)%tail%nldepart(:)=-huge(zero)
             obsdiags(i_cldch_ob_type,ibin)%tail%tldepart(:)=zero
             obsdiags(i_cldch_ob_type,ibin)%tail%wgtjo=-huge(zero)
             obsdiags(i_cldch_ob_type,ibin)%tail%obssen(:)=zero
  
             n_alloc(ibin) = n_alloc(ibin) +1
             my_diag => obsdiags(i_cldch_ob_type,ibin)%tail
             my_diag%idv = is
             my_diag%iob = i
             my_diag%ich = 1
          else
             if (.not.associated(obsdiags(i_cldch_ob_type,ibin)%tail)) then
                obsdiags(i_cldch_ob_type,ibin)%tail => obsdiags(i_cldch_ob_type,ibin)%head
             else
                obsdiags(i_cldch_ob_type,ibin)%tail => obsdiags(i_cldch_ob_type,ibin)%tail%next
             end if
             if (obsdiags(i_cldch_ob_type,ibin)%tail%indxglb/=i) then
                write(6,*)'setupcldch: index error'
                call stop2(297)
             end if
          endif
       endif
  
       if(.not.in_curbin) cycle
  
  ! Interpolate to get cldch at obs location/time
       call tintrp2a11(this%ges_cldch,cldchges,dlat,dlon,dtime,hrdifsig,&
          mype,nfldsig)
  
  ! Adjust observation error
       ratio_errors=error/data(ier,i)
       error=one/error
  
       ddiff=data(icldch,i)-cldchges
  
  ! If requested, setup for single obs test.
       if (oneobtest) then
          ddiff=maginnov
          error=one/magoberr
          ratio_errors=one
       endif
  
  !    Gross check using innovation normalized by error
       if (abs(data(icldch,i)-rmiss_single) >= tiny_r_kind ) then  !MIGHT WANT TO IMPROVE THIS. MPondeca /17Jul2015
          obserror = one/max(ratio_errors*error,tiny_r_kind)
          obserrlm = max(cermin(ikx),min(cermax(ikx),obserror))
          residual = abs(ddiff)
          ratio    = residual/obserrlm
          if (ratio> cgross(ikx) .or. ratio_errors < tiny_r_kind) then
             if (luse(i)) awork(6) = awork(6)+one
             error = zero
             ratio_errors=zero
          else
             ratio_errors=ratio_errors/sqrt(dup(i))
          end if
       else    ! missing data
          error = zero
          ratio_errors=zero
       end if
  
       if (ratio_errors*error <=tiny_r_kind) muse(i)=.false.
       if (nobskeep>0 .and. luse_obsdiag) muse(i)=obsdiags(i_cldch_ob_type,ibin)%tail%muse(nobskeep)
  
  !    Compute penalty terms (linear & nonlinear qc).
       val      = error*ddiff
       if(luse(i))then
          val2     = val*val
          exp_arg  = -half*val2
          rat_err2 = ratio_errors**2
          if (cvar_pg(ikx) > tiny_r_kind .and. error > tiny_r_kind) then
             arg  = exp(exp_arg)
             wnotgross= one-cvar_pg(ikx)
             cg_cldch=cvar_b(ikx)
             wgross = cg_term*cvar_pg(ikx)/(cg_cldch*wnotgross)
             term = log((arg+wgross)/(one+wgross))
             wgt  = one-wgross/(arg+wgross)
             rwgt = wgt/wgtlim
          else
             term = exp_arg
             wgt  = wgtlim
             rwgt = wgt/wgtlim
          endif
          valqc = -two*rat_err2*term
  
  !       Accumulate statistics for obs belonging to this task
          if (muse(i)) then
             if(rwgt < one) awork(21) = awork(21)+one
             awork(4)=awork(4)+val2*rat_err2
             awork(5)=awork(5)+one
             awork(22)=awork(22)+valqc
             nn=1
           else
             nn=2                                     !rejected obs
             if(ratio_errors*error >=tiny_r_kind)nn=3 !monitored obs
          end if
  
          ress   = ddiff*scale
          ressw2 = ress*ress
  
          if (abs(data(icldch,i)-rmiss_single) >=tiny_r_kind) then
             bwork(1,ikx,1,nn)  = bwork(1,ikx,1,nn)+one           ! count
             bwork(1,ikx,2,nn)  = bwork(1,ikx,2,nn)+ress          ! (o-g)
             bwork(1,ikx,3,nn)  = bwork(1,ikx,3,nn)+ressw2        ! (o-g)**2
             bwork(1,ikx,4,nn)  = bwork(1,ikx,4,nn)+val2*rat_err2 ! penalty
             bwork(1,ikx,5,nn)  = bwork(1,ikx,5,nn)+valqc         ! nonlin qc penalty
          end if
  
       endif
  
       if(luse_obsdiag)then
          obsdiags(i_cldch_ob_type,ibin)%tail%luse=luse(i)
          obsdiags(i_cldch_ob_type,ibin)%tail%muse(jiter)=muse(i)
          obsdiags(i_cldch_ob_type,ibin)%tail%nldepart(jiter)=ddiff
          obsdiags(i_cldch_ob_type,ibin)%tail%wgtjo= (error*ratio_errors)**2
       end if
  
  !    If obs is "acceptable", load array with obs info for use
  !    in inner loop minimization (int* and stp* routines)
       if (.not. last .and. muse(i)) then
  
          if(.not. associated(cldchhead(ibin)%head))then
             allocate(cldchhead(ibin)%head,stat=istat)
             if(istat /= 0)write(6,*)' failure to write cldchhead '
             cldchtail(ibin)%head => cldchhead(ibin)%head
          else
             allocate(cldchtail(ibin)%head%llpoint,stat=istat)
             if(istat /= 0)write(6,*)' failure to write cldchtail%llpoint '
             cldchtail(ibin)%head => cldchtail(ibin)%head%llpoint
          end if
  
          m_alloc(ibin) = m_alloc(ibin) + 1
          my_head => cldchtail(ibin)%head
          my_head%idv = is
          my_head%iob = i
  
  !       Set (i,j) indices of guess gridpoint that bound obs location
          call get_ij(mm1,dlat,dlon,cldchtail(ibin)%head%ij(1),cldchtail(ibin)%head%wij(1))
  
          cldchtail(ibin)%head%res     = ddiff
          cldchtail(ibin)%head%err2    = error**2
          cldchtail(ibin)%head%raterr2 = ratio_errors**2    
          cldchtail(ibin)%head%time    = dtime
          cldchtail(ibin)%head%b       = cvar_b(ikx)
          cldchtail(ibin)%head%pg      = cvar_pg(ikx)
          cldchtail(ibin)%head%luse    = luse(i)
          if(luse_obsdiag)then
             cldchtail(ibin)%head%diags => obsdiags(i_cldch_ob_type,ibin)%tail
   
             my_head => cldchtail(ibin)%head
             my_diag => cldchtail(ibin)%head%diags
             if(my_head%idv /= my_diag%idv .or. &
                my_head%iob /= my_diag%iob ) then
                call perr(myname,'mismatching %[head,diags]%(idv,iob,ibin) =', &
                       (/is,i,ibin/))
               call perr(myname,'my_head%(idv,iob) =',(/my_head%idv,my_head%iob/))
               call perr(myname,'my_diag%(idv,iob) =',(/my_diag%idv,my_diag%iob/))
               call die(myname)
             endif
          endif
       endif
  
  
  !    Save stuff for diagnostic output
       if(conv_diagsave .and. luse(i))then
          ii=ii+1
          rstation_id     = data(id,i)
          cdiagbuf(ii)    = station_id         ! station id
   
          rdiagbuf(1,ii)  = ictype(ikx)        ! observation type
          rdiagbuf(2,ii)  = icsubtype(ikx)     ! observation subtype
   
          rdiagbuf(3,ii)  = data(ilate,i)      ! observation latitude (degrees)
          rdiagbuf(4,ii)  = data(ilone,i)      ! observation longitude (degrees)
          rdiagbuf(5,ii)  = data(istnelv,i)    ! station elevation (meters)
          rdiagbuf(6,ii)  = rmiss_single       ! observation pressure (hPa)
          rdiagbuf(7,ii)  = data(iobshgt,i)    ! observation height (meters)
          rdiagbuf(8,ii)  = dtime-time_offset  ! obs time (hours relative to analysis time)
  
          rdiagbuf(9,ii)  = data(iqc,i)        ! input prepbufr qc or event mark
          rdiagbuf(10,ii) = rmiss_single       ! setup qc or event mark
          rdiagbuf(11,ii) = data(iuse,i)       ! read_prepbufr data usage flag
          if(muse(i)) then
             rdiagbuf(12,ii) = one             ! analysis usage flag (1=use, -1=not used)
          else
             rdiagbuf(12,ii) = -one
          endif
  
          err_input = data(ier,i)
          err_adjst = data(ier,i)
          if (ratio_errors*error>tiny_r_kind) then
             err_final = one/(ratio_errors*error)
          else
             err_final = huge_single
          endif
   
          errinv_input = huge_single
          errinv_adjst = huge_single
          errinv_final = huge_single
          if (err_input>tiny_r_kind) errinv_input = one/err_input
          if (err_adjst>tiny_r_kind) errinv_adjst = one/err_adjst
          if (err_final>tiny_r_kind) errinv_final = one/err_final
  
          rdiagbuf(13,ii) = rwgt               ! nonlinear qc relative weight
          rdiagbuf(14,ii) = errinv_input       ! prepbufr inverse obs error (m**-1)
          rdiagbuf(15,ii) = errinv_adjst       ! read_prepbufr inverse obs error (m**-1)
          rdiagbuf(16,ii) = errinv_final       ! final inverse observation error (m**-1)
   
          rdiagbuf(17,ii) = data(icldch,i)     ! CLDCH observation (m)
          rdiagbuf(18,ii) = ddiff              ! obs-ges used in analysis (m)
          rdiagbuf(19,ii) = data(icldch,i)-cldchges! obs-ges w/o bias correction (m) (future slot)
   
          rdiagbuf(20,ii) = rmiss_single       ! type of measurement
  
          rdiagbuf(21,ii) = data(idomsfc,i)    ! dominate surface type
          rdiagbuf(22,ii) = data(izz,i)        ! model terrain at observation location
          r_prvstg        = data(iprvd,i)
          cprvstg(ii)     = c_prvstg           ! provider name
          r_sprvstg       = data(isprvd,i)
          csprvstg(ii)    = c_sprvstg          ! subprovider name
  
          ioff=ioff0
          if (lobsdiagsave) then
             do jj=1,miter 
                ioff=ioff+1 
                if (obsdiags(i_cldch_ob_type,ibin)%tail%muse(jj)) then
                   rdiagbuf(ioff,ii) = one
                else
                   rdiagbuf(ioff,ii) = -one
                endif
             enddo
             do jj=1,miter+1
                ioff=ioff+1
                rdiagbuf(ioff,ii) = obsdiags(i_cldch_ob_type,ibin)%tail%nldepart(jj)
             enddo
             do jj=1,miter
                ioff=ioff+1
                rdiagbuf(ioff,ii) = obsdiags(i_cldch_ob_type,ibin)%tail%tldepart(jj)
             enddo
             do jj=1,miter
                ioff=ioff+1
                rdiagbuf(ioff,ii) = obsdiags(i_cldch_ob_type,ibin)%tail%obssen(jj)
             enddo
          endif
   
       end if
  
  
    end do
  
  ! Release memory of local guess arrays
    call this%final_vars_
  
  ! Write information to diagnostic file
    if(conv_diagsave .and. ii>0)then
       call dtime_show(myname,'diagsave:cldch',i_cldch_ob_type)
       write(7)'cei',nchar,nreal,ii,mype,ioff0
       write(7)cdiagbuf(1:ii),rdiagbuf(:,1:ii)
       deallocate(cdiagbuf,rdiagbuf)
  
       write(7)cprvstg(1:ii),csprvstg(1:ii)
       deallocate(cprvstg,csprvstg)
    end if
  
  ! End of routine
  
    return
end subroutine setupcldch
 
   subroutine init_vars_cldch(this)
   use gsi_bundlemod, only : gsi_bundlegetpointer
   use gsi_metguess_mod, only : gsi_metguess_bundle
   use guess_grids, only: nfldsig
      implicit none
      class(setupcldch_class)                              , intent(inout) :: this
 
   real(r_kind),dimension(:,:  ),pointer:: rank2=>NULL()
   character(len=5) :: varname
   integer(i_kind) ifld, istatus
 
 ! If require guess vars available, extract from bundle ...
   if(size(gsi_metguess_bundle)==nfldsig) then
 !    get ps ...
      varname='ps'
      call gsi_bundlegetpointer(gsi_metguess_bundle(1),trim(varname),rank2,istatus)
      if (istatus==0) then
          if(allocated(this%ges_ps))then
             write(6,*) trim(this%myname), ': ', trim(varname), ' already incorrectly alloc '
             call stop2(999)
          endif
          allocate(this%ges_ps(size(rank2,1),size(rank2,2),nfldsig))
          this%ges_ps(:,:,1)=rank2
          do ifld=2,nfldsig
             call gsi_bundlegetpointer(gsi_metguess_bundle(ifld),trim(varname),rank2,istatus)
             this%ges_ps(:,:,ifld)=rank2
          enddo
      else
          write(6,*) trim(this%myname),': ', trim(varname), ' not found in met bundle, ier= ',istatus
          call stop2(999)
      endif
 !    get cldch ...
      varname='cldch'
      call gsi_bundlegetpointer(gsi_metguess_bundle(1),trim(varname),rank2,istatus)
      if (istatus==0) then
          if(allocated(this%ges_cldch))then
             write(6,*) trim(this%myname), ': ', trim(varname), ' already incorrectly alloc '
             call stop2(999)
          endif
          allocate(this%ges_cldch(size(rank2,1),size(rank2,2),nfldsig))
          this%ges_cldch(:,:,1)=rank2
          do ifld=2,nfldsig
             call gsi_bundlegetpointer(gsi_metguess_bundle(ifld),trim(varname),rank2,istatus)
             this%ges_cldch(:,:,ifld)=rank2
          enddo
      else
          write(6,*) trim(this%myname),': ', trim(varname), ' not found in met bundle, ier= ',istatus
          call stop2(999)
      endif
 !    get z ...
      varname='z'
      call gsi_bundlegetpointer(gsi_metguess_bundle(1),trim(varname),rank2,istatus)
      if (istatus==0) then
          if(allocated(this%ges_z))then
             write(6,*) trim(this%myname), ': ', trim(varname), ' already incorrectly alloc '
             call stop2(999)
          endif
          allocate(this%ges_z(size(rank2,1),size(rank2,2),nfldsig))
          this%ges_z(:,:,1)=rank2
          do ifld=2,nfldsig
             call gsi_bundlegetpointer(gsi_metguess_bundle(ifld),trim(varname),rank2,istatus)
             this%ges_z(:,:,ifld)=rank2
          enddo
      else
          write(6,*) trim(this%myname),': ', trim(varname), ' not found in met bundle, ier= ',istatus
          call stop2(999)
      endif
   else
      write(6,*) trim(this%myname), ': inconsistent vector sizes (nfldsig,size(metguess_bundle) ',&
                  nfldsig,size(gsi_metguess_bundle)
      call stop2(999)
   endif
   end subroutine init_vars_cldch
 
end module setupcldch_mod
