module stpdbzmod

!$$$ module documentation block
!           .      .    .                                       .
! module:   stpdbzmod    module for stpdbz_search and stpdbz
!  prgmmr:
!
! abstract: module for stpdbz_search and stpdbz
!
! program history log:
!   2017-05-12  Y. Wang and X. Wang - add adjoint of reflectivity operator (Wang and Wang 2017 MWR), POC: xuguang.wang@ou.edu
!   2019-08-21  kbathmann - split into stpdbz and stpdbz_search
!
! subroutines included:
!   sub stpdbz,stpdbz_search
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
use kinds, only: r_kind,i_kind,r_quad
use qcmod, only: nlnqc_iter,varqc_iter
use constants, only: half,one,two,tiny_r_kind,cg_term,zero_quad
use m_obsNode, only: obsNode
use m_dbzNode , only: dbzNode
use m_dbzNode , only: dbzNode_typecast
use m_dbzNode , only: dbzNode_nextcast
implicit none

PRIVATE
PUBLIC stpdbz,stpdbz_search

contains

subroutine stpdbz_search(dbzhead,rval,out,sges,nstep)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    stpdbz_search  compute the value of the current search direction
!                               in obs space, as well as the contribution to
!                               penalty, with nonlinear qc
!   prgmmr: derber           org: np23                date: 1991-02-26
!
! abstract: calculate search direction, penalty and contribution to stepsize from radar winds
!
! program history log:
!   2019-08-21 kbathmann split the computation of val into its own subroutine
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use gsi_bundlemod, only: gsi_bundle
  use gsi_bundlemod, only: gsi_bundlegetpointer
  use gridmod, only: wrf_mass_regional
  use control_vectors, only : dbz_exist

  implicit none

! Declare passed variables
  class(obsNode), pointer             ,intent(in   ) :: dbzhead
  integer(i_kind)                     ,intent(in   ) :: nstep
  real(r_quad),dimension(max(1,nstep)),intent(inout) :: out
  type(gsi_bundle)                    ,intent(in   ) :: rval
  real(r_kind),dimension(max(1,nstep)),intent(in   ) :: sges

! Declare local variables
  integer(i_kind) ier,istatus
  integer(i_kind) j1,j2,j3,j4,j5,j6,j7,j8,kk
  real(r_kind) w1,w2,w3,w4,w5,w6,w7,w8
  real(r_kind) valqr, valqs, valqg, valdbz
  real(r_kind) qrcur, qscur, qgcur, dbzcur
  real(r_kind) cg_dbz,dbz,wgross,wnotgross
  real(r_kind),dimension(max(1,nstep))::pen
  real(r_kind) pg_dbz
  real(r_kind),pointer,dimension(:) :: rqr,rqs,rqg,rdbz
  type(dbzNode), pointer :: dbzptr

  out=zero_quad

!  If no dbz data return
  if(.not. associated(dbzhead))return

! Retrieve pointers
! Simply return if any pointer not found
  ier=0
  if(dbz_exist)then
    call gsi_bundlegetpointer(rval,'dbz',rdbz,istatus);ier=istatus+ier
  else
    call gsi_bundlegetpointer(rval,'qr',rqr,istatus);ier=istatus+ier
    if (wrf_mass_regional) then
      call gsi_bundlegetpointer(rval,'qg',rqg,istatus);ier=istatus+ier
    end if
  end if
  if(ier/=0)return

  dbzptr => dbzNode_typecast(dbzhead)
  do while (associated(dbzptr))
     if(dbzptr%luse)then
        j1=dbzptr%ij(1)
        j2=dbzptr%ij(2)
        j3=dbzptr%ij(3)
        j4=dbzptr%ij(4)
        j5=dbzptr%ij(5)
        j6=dbzptr%ij(6)
        j7=dbzptr%ij(7)
        j8=dbzptr%ij(8)
        w1=dbzptr%wij(1)
        w2=dbzptr%wij(2)
        w3=dbzptr%wij(3)
        w4=dbzptr%wij(4)
        w5=dbzptr%wij(5)
        w6=dbzptr%wij(6)
        w7=dbzptr%wij(7)
        w8=dbzptr%wij(8)

        if( dbz_exist )then
          dbzptr%val= w1* rdbz(j1)+w2*rdbz(j2)+w3*rdbz(j3)+w4*rdbz(j4)+ &
                  w5* rdbz(j5)+w6*rdbz(j6)+w7*rdbz(j7)+w8*rdbz(j8)
        else
          valqr=(w1* rqr(j1)+w2* rqr(j2)+w3* rqr(j3)+w4* rqr(j4)+       &
                 w5* rqr(j5)+w6* rqr(j6)+w7* rqr(j7)+w8* rqr(j8))
          if (wrf_mass_regional)then
            valqs=(w1* rqs(j1)+w2* rqs(j2)+w3* rqs(j3)+w4* rqs(j4)+ &
                   w5* rqs(j5)+w6* rqs(j6)+w7* rqs(j7)+w8* rqs(j8))
            valqg=(w1* rqg(j1)+w2* rqg(j2)+w3* rqg(j3)+w4* rqg(j4)+ &
                   w5* rqg(j5)+w6* rqg(j6)+w7* rqg(j7)+w8* rqg(j8))
            dbzptr%val = valqr * dbzptr%jqr + valqs *  dbzptr%jqs +     &
                     valqg * dbzptr%jqg
          end if!KAB this if statement is weird  
        end if

        do kk=1,nstep
           dbz=dbzptr%val2+sges(kk)*dbzptr%val
           pen(kk)=dbz*dbz*dbzptr%err2
        end do

!  Modify penalty term if nonlinear QC
        if (nlnqc_iter .and. dbzptr%pg > tiny_r_kind .and.  &
                             dbzptr%b  > tiny_r_kind) then
           pg_dbz=dbzptr%pg*varqc_iter
           cg_dbz=cg_term/dbzptr%b
           wnotgross= one-pg_dbz
           wgross = pg_dbz*cg_dbz/wnotgross
           do kk=1,max(1,nstep)
              pen(kk)= -two*log((exp(-half*pen(kk)) + wgross)/(one+wgross))
           end do
        endif

        out(1) = out(1)+pen(1)*dbzptr%raterr2
        do kk=2,nstep
           out(kk) = out(kk)+(pen(kk)-pen(1))*dbzptr%raterr2
        end do
     end if !luse
     dbzptr => dbzNode_nextcast(dbzptr)
  end do !while associated(dbzptr)
  return
end subroutine stpdbz_search
subroutine stpdbz(dbzhead,out,sges,nstep)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    stpdbz       calculate penalty and contribution to
!                            stepsize with nonlinear qc added.
!   prgmmr: derber           org: np23                date: 1991-02-26
!
! abstract: calculate penalty and contribution to stepsize from radar winds
!
! program history log:
!   1991-02-26  derber
!   1999-11-22  yang
!   2004-07-29  treadon - add only to module use, add intent in/out
!   2004-10-07  parrish - add nonlinear qc option
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  implicit none

! Declare passed variables
  class(obsNode), pointer             ,intent(in   ) :: dbzhead
  integer(i_kind)                     ,intent(in   ) :: nstep
  real(r_quad),dimension(max(1,nstep)),intent(inout) :: out
  real(r_kind),dimension(max(1,nstep)),intent(in   ) :: sges

! Declare local variables
  integer(i_kind) kk
  real(r_kind) cg_dbz,dbz,wgross,wnotgross
  real(r_kind),dimension(max(1,nstep))::pen
  real(r_kind) pg_dbz
  type(dbzNode), pointer :: dbzptr

  out=zero_quad

!  If no dbz data return
  if(.not. associated(dbzhead))return

  dbzptr => dbzNode_typecast(dbzhead)
  do while (associated(dbzptr))
     if(dbzptr%luse)then
        if(nstep > 0)then
           do kk=1,nstep
              dbz=dbzptr%val2+sges(kk)*dbzptr%val
              pen(kk)=dbz*dbz*dbzptr%err2
           end do
        else
           pen(1)=dbzptr%res*dbzptr%res*dbzptr%err2
        end if
!  Modify penalty term if nonlinear QC
        if (nlnqc_iter .and. dbzptr%pg > tiny_r_kind .and.  &
                             dbzptr%b  > tiny_r_kind) then
           pg_dbz=dbzptr%pg*varqc_iter
           cg_dbz=cg_term/dbzptr%b
           wnotgross= one-pg_dbz
           wgross = pg_dbz*cg_dbz/wnotgross
           do kk=1,max(1,nstep)
              pen(kk)= -two*log((exp(-half*pen(kk)) + wgross)/(one+wgross))
           end do
        endif

        out(1) = out(1)+pen(1)*dbzptr%raterr2
        do kk=2,nstep
           out(kk) = out(kk)+(pen(kk)-pen(1))*dbzptr%raterr2
        end do
     end if !luse
     dbzptr => dbzNode_nextcast(dbzptr)
  end do !while associated(dbzptr)
  return
end subroutine stpdbz

end module stpdbzmod
