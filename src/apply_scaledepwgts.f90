!abstract interface 
! function fwgtofwvlen (rvlft,rvrgt,rcons,rlen,rinput)
!          real (r_kind):: fwgtoffwvlen
!          real(r_kind):: rvlft,rvrgt,rcons,rlen,rinput
! end function fwgtofwvlen
!end interface 
!$$$  subprogram documentation block
 function fwgtofwvlen (rvlft,rvrgt,rcons,rlen,rinput)
           use kinds, only: r_kind,i_kind,r_single
          real (r_kind):: fwgtofwvlen
          real(r_kind):: rvlft,rvrgt,rcons,rlen,rinput
          real (r_kind):: rtem1
          if(rinput.gt.rvlft.and.rinput.lt.rvrgt) then
              fwgtofwvlen=rcons
          else
              rtem1=min(abs(rinput-rvlft),abs(rinput-rvrgt))
              fwgtofwvlen=rcons*exp(-rtem1/rlen)
          endif
        
       
 end function fwgtofwvlen
!                .      .    .                                       .
!
!$$$

subroutine init_mult_spc_wgts(jcap_in)
  use kinds, only: r_kind,i_kind,r_single
  use hybrid_ensemble_parameters,only: s_ens_hv,sp_loc,grd_ens,grd_loc,sp_ens,n_ens,p_sploc2ens,grd_sploc
  use hybrid_ensemble_parameters,only: use_localization_grid
  use gridmod,only: use_sp_eqspace
  use general_specmod, only: general_init_spec_vars
  use constants, only: zero,half,one,two,three,rearth,pi
  use constants, only: rad2deg
  use mpimod, only: mype
  use general_sub2grid_mod, only: general_sub2grid_create_info
  use egrid2agrid_mod,only: g_create_egrid2agrid
  use general_sub2grid_mod, only: sub2grid_info
  use gsi_io, only: verbose
  use hybrid_ensemble_parameters,only: naensgrp
  use hybrid_ensemble_parameters, only: nsclgrp
  use hybrid_ensemble_parameters, only: spc_multwgt,spcwgt_params
  implicit none

  integer(i_kind),intent(in   ) :: jcap_in
  real(r_kind),allocatable::totwvlength(:)

  integer(i_kind) i,ii,j,k,l,n,jcap,kk,nsigend
  character(5) mapname
  integer ig
  real(r_kind) rwvint,rtem1,rtem2
  real (r_kind):: fwgtofwvlen


!    make sure s_ens_hv is within allowable range  ( pi*rearth*.001/jcap_in <= s_ens_hv <= 5500 )
  allocate(totwvlength(jcap))

   
  rwvint=2*pi*rearth*.001_r_kind/jcap_in
  do i=1,jcap_in
  totwvlength(i)=i*rwvint
  enddo
 do i=1,jcap
  rtem1=0
  do ig=1,nsclgrp
  spc_multwgt(i,ig)=fwgtofwvlen(spcwgt_params(1,ig),spcwgt_params(2,ig),spcwgt_params(3,ig),spcwgt_params(4,ig),totwvlength(i)) !fwv2wgt(twvlength)
  rtem1=rtem1+spc_multwgt(i,ig)
   enddo
  rtem2=1-rtem1
  if(abs(rtem2).gt.zero) then 
   do ig=1,nsclgrp
   spc_multwgt(i,ig)=spc_multwgt(i,ig)/rtem1*rtem2+spc_multwgt(i,ig)
   enddo
  endif
   
  enddo
  
  deallocate(totwvlength)
  return

end subroutine init_mult_spc_wgts



subroutine apply_scaledepwgts(grd_in,sp_in,wbundle,spwgts,wbundle2)  
!   2017-03-30  J. Kay, X. Wang - copied from Kleist's apply_scaledepwgts and
!                                 add the calculation of scale-dependent weighting for mixed resolution ensemble  
!                                 POC: xuguang.wang@ou.edu
!
  use constants, only:  one
  use control_vectors, only: nrf_var,cvars2d,cvars3d,control_vector
  use kinds, only: r_kind,i_kind
  use mpimod, only: mype,nvar_id
  use hybrid_ensemble_parameters, only: oz_univ_static
  use general_specmod, only: general_spec_multwgt
  use gsi_bundlemod, only: gsi_bundle
  use general_sub2grid_mod, only: general_sub2grid,general_grid2sub   
  use general_specmod, only: spec_vars
  use general_sub2grid_mod, only: sub2grid_info
   use mpimod, only: mpi_comm_world,mype,npe,ierror
  implicit none

! Declare passed variables
  type(gsi_bundle),intent(in) :: wbundle
  type(gsi_bundle),intent(inout) :: wbundle2
  type(spec_vars),intent (in):: sp_in
  type(sub2grid_info),intent(in)::grd_in
  real(r_kind),dimension(0:sp_in%jcap),intent(in):: spwgts

! Declare local variables
  integer(i_kind) ii,iflg,kk

  real(r_kind),dimension(grd_in%lat2,grd_in%lon2):: slndt,sicet,sst
  real(r_kind),dimension(grd_in%nlat*grd_in%nlon*grd_in%nlevs_alloc)      :: hwork
  real(r_kind),dimension(grd_in%nlat,grd_in%nlon,grd_in%nlevs_alloc)      :: work
  real(r_kind),dimension(grd_in%nlat,grd_in%nlon,grd_in%nlevs_alloc)      :: work1
  real(r_kind),dimension(sp_in%nc):: spc1

  iflg=1
! Beta1 first
! Get from subdomains to
  call general_sub2grid(grd_in,wbundle%values,hwork)
  work=reshape(hwork,(/grd_in%nlat,grd_in%nlon,grd_in%nlevs_alloc/))
  work1=work
! Transfer work to spectral space
  do kk=1,grd_in%nlevs_alloc      

! Shut off this bit if univariate ozone or surface temperature
     if ( (nrf_var(nvar_id(kk))=='oz').or.(nrf_var(nvar_id(kk))=='OZ').and.oz_univ_static ) then
        cycle
     elseif ( (nrf_var(nvar_id(kk))=='sst').or.(nrf_var(nvar_id(kk))=='SST') ) then
        cycle
     elseif ( (nrf_var(nvar_id(kk))=='stl').or.(nrf_var(nvar_id(kk))=='STL') ) then
        cycle
     elseif ( (nrf_var(nvar_id(kk))=='sti').or.(nrf_var(nvar_id(kk))=='STI') ) then
        cycle
     end if

! Transform from physical space to spectral space   
     call general_g2s0(grd_in,sp_in,spc1,work(:,:,kk))
  if(mype == 0) write(6,*) 'jk_apply0=', spc1
  if(mype == 0) write(6,*) 'jk_apply0_spc1=', shape(spc1)
  if(mype == 0) write(6,*) 'jk_apply0_spc2=', sp_in%jcap
  if(mype == 0) write(6,*) 'jk_apply0_spc2=', sp_in%nc
  if(mype == 0) write(6,*) 'jk_apply0_spc3=', spwgts 

! Apply spectral weights
  call general_spec_multwgt(sp_in,spc1,spwgts)
  !if(mype == 0) write(6,*) 'jk_apply1=', spwgts
! Transform back to physical space
    call general_s2g0(grd_in,sp_in,spc1,work(:,:,kk))
  work1(:,:,kk)=work(:,:,kk)-work1(:,:,kk)
  if(mype ==0) write(6,*)'thinkdeb150 max diff is ',maxval(work1(:,:,kk))
  end do

! Transfer work back to subdomains
  hwork=reshape(work,(/grd_in%nlat*grd_in%nlon*grd_in%nlevs_alloc/))
  call general_grid2sub(grd_in,hwork,wbundle2%values)    

  return
end subroutine apply_scaledepwgts

subroutine apply_scaledepwgts_ad(grd_in,sp_in,wbundle,spwgts)
  use constants, only:  one, zero
  use control_vectors, only: nrf_var,cvars2d,cvars3d,control_vector
  use kinds, only: r_kind,i_kind
  use mpimod, only: mype,nvar_id
  use hybrid_ensemble_parameters, only: oz_univ_static
  use general_specmod, only: general_spec_multwgt
  use general_specmod, only: spec_vars
!cltorgl  use gridmod, only: ,nlat,nlon,nnnn1o,lat2,lon2,nsig
  use gsi_bundlemod, only: gsi_bundle
  use general_sub2grid_mod, only: general_sub2grid,general_grid2sub   
  use general_sub2grid_mod, only: sub2grid_info
  implicit none

! Declare passed variables
  type(gsi_bundle),intent(inout) :: wbundle
  type(spec_vars),intent (in):: sp_in
  type(sub2grid_info),intent(in)::grd_in
  real(r_kind),dimension(sp_in%jcap),intent(in):: spwgts

! Declare local variables
  integer(i_kind) ii,iflg,kk

  real(r_kind),dimension(grd_in%lat2,grd_in%lon2):: slndt,sicet,sst
  real(r_kind),dimension(grd_in%nlat*grd_in%nlon*grd_in%nlevs_alloc)      :: hwork
  real(r_kind),dimension(grd_in%nlat,grd_in%nlon,grd_in%nlevs_alloc)      :: work
  real(r_kind),dimension(grd_in%nlat,grd_in%nlon,grd_in%nlevs_alloc)      :: work1
  real(r_kind),dimension(sp_in%nc):: spc1

  iflg=1
! Get from subdomains to
  call general_sub2grid(grd_in,wbundle%values,hwork)
  work=reshape(hwork,(/grd_in%nlat,grd_in%nlon,grd_in%nlevs_alloc/))
  work1=work
! Transfer work to spectral space
  do kk=1,grd_in%nlevs_alloc     

! Shut off this bit if univariate ozone or surface temperature
     if ( (nrf_var(nvar_id(kk))=='oz').or.(nrf_var(nvar_id(kk))=='OZ').and.oz_univ_static ) then
        cycle
     elseif ( (nrf_var(nvar_id(kk))=='sst').or.(nrf_var(nvar_id(kk))=='SST') ) then
        cycle
     elseif ( (nrf_var(nvar_id(kk))=='stl').or.(nrf_var(nvar_id(kk))=='STL') ) then
        cycle
     elseif ( (nrf_var(nvar_id(kk))=='sti').or.(nrf_var(nvar_id(kk))=='STI') ) then
        cycle
     end if
     spc1(:)=zero

! Adjoint of s2g
     call general_s2g0_ad(grd_in,sp_in,spc1,work(:,:,kk))

! Spectral weights
!cltthinkdeb     call general_spec_multwgt(sp_in,spc1,spwgts)
  if(mype == 0) write(6,*) 'jk_apply_ad=', spwgts

! Adjoint of g2s
  call general_g2s0_ad(grd_in,sp_in,spc1,work(:,:,kk))
  work1(:,:,kk)=work(:,:,kk)-work1(:,:,kk)
  if(mype ==0) write(6,*)'thinkdeb250 max diff is ',maxval(work1(:,:,kk))

  end do
! Transfer work back to subdomains
  hwork=reshape(work,(/grd_in%nlat*grd_in%nlon*grd_in%nlevs_alloc/))
!cltthinkdeb  call general_grid2sub(grd_in,hwork,wbundle%values)    

  return
end subroutine apply_scaledepwgts_ad
