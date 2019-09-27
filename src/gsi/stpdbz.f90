module stpdbzmod
!$$$ module documentation block
!           .      .    .                                       .
! module:   stpdbzmod    module for stpdbz and its tangent linear stpdbz_tl
!
! abstract: module for stpdbz and its tangent linear stpdbz_tl
!
! program history log:
!   2005-05-19  Yanqiu zhu - wrap stprw and its tangent linear stprw_tl into one module
!   2005-11-16  Derber - remove interfaces
!   2008-12-02  Todling - remove stprw_tl
!   2009-08-12  lueken - update documentation
!   2010-05-13  todling - uniform interface across stp routines
!   2016-09-xx  g.zhao - new ob dbz into GSIv3.5 (based on my stdpdbz in v3.3)
!                      - stpdbzmod is based on stpqmod and stprw module
!                      - using tangent linear dbz operator 
!                      - working with log(qr/qs/qg) (no modification to stpdbz)
!   2017-05-12  Y. Wang and X. Wang - add adjoint of reflectivity operator (Wang and Wang 2017 MWR), POC: xuguang.wang@ou.edu
!   2019-02-19  ctong  - modified to comply with new type structure
!
! subroutines included:
!   sub stpdbz
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

implicit none

PRIVATE
PUBLIC stpdbz

contains

subroutine stpdbz(dbzhead,rval,sval,out,sges,nstep)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    stpdbz       calculate penalty and contribution to
!                            stepsize with nonlinear qc added.
!   prgmmr: derber           org: np23                date: 1991-02-26
!
! abstract: calculate penalty and contribution to stepsize from radar reflectivity
!
! program history log:
!   1991-02-26  derber
!   1999-11-22  yang
!   2004-07-29  treadon - add only to module use, add intent in/out
!   2004-10-07  parrish - add nonlinear qc option
!   2005-04-11  treadon - merge stprw and stprw_qc into single routine
!   2005-08-02  derber  - modify for variational qc parameters for each ob
!   2005-09-28  derber  - consolidate location and weight arrays
!   2007-03-19  tremolet - binning of observations
!   2007-07-28  derber  - modify to use new inner loop obs data structure
!                       - unify NL qc
!   2007-02-15  rancic - add foto
!   2007-06-04  derber  - use quad precision to get reproducability over number of processors  
!   2008-06-02  safford - rm unused var and uses
!   2008-12-03  todling - changed handling of ptr%time
!   2010-01-04  zhang,b - bug fix: accumulate penalty for multiple obs bins
!   2010-05-13  todling - update to use gsi_bundle
!   2016-09-xx  G.Zhao  - dbz
!
!   input argument list:
!     dbzhead
!     rqr      - search direction for qr
!     rqs     - search direction for qs
!     rqg     - search direction for qg
!     sqr      - analysis increment for qr
!     sqs     - analysis increment for qs
!     sqg     - analysis increment for qg
!     sges     - step size estimates (nstep)
!     nstep    - number of step sizes (== 0 means use outer iteration value)
!
!   output argument list     - output for step size calculation
!     out(1:nstep)   - penalty from radar reflectivity sges(1:nstep)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind,r_quad
  use constants, only: half,one,two,tiny_r_kind,cg_term,zero_quad,r3600
  use gsi_bundlemod, only: gsi_bundle
  use gsi_bundlemod, only: gsi_bundlegetpointer
  use gridmod, only: wrf_mass_regional
  use control_vectors, only : dbz_exist
  use m_obsNode, only: obsNode
  use m_dbzNode , only: dbzNode
  use m_dbzNode , only: dbzNode_typecast
  use m_dbzNode , only: dbzNode_nextcast
  use qcmod, only: nlnqc_iter,varqc_iter,njqc,vqc !CAPS
  use caps_radaruse_mod, only: l_use_dbz_caps  ! CAPS

  implicit none

! Declare passed variables
  class(obsNode), pointer             ,intent(in   ) :: dbzhead
  integer(i_kind)                     ,intent(in   ) :: nstep
  real(r_quad),dimension(max(1,nstep)),intent(inout) :: out
  type(gsi_bundle)                    ,intent(in   ) :: rval,sval
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
  real(r_kind),pointer,dimension(:) :: sqr,sqs,sqg,sdbz
  real(r_kind),pointer,dimension(:) :: rqr,rqs,rqg,rdbz
  type(dbzNode), pointer :: dbzptr

  out=zero_quad

!  If no dbz data return
  if(.not. associated(dbzhead))return

! Retrieve pointers
! Simply return if any pointer not found
  ier=0
  if(dbz_exist)then
    call gsi_bundlegetpointer(sval,'dbz',sdbz,istatus);ier=istatus+ier
    call gsi_bundlegetpointer(rval,'dbz',rdbz,istatus);ier=istatus+ier
  else
    call gsi_bundlegetpointer(sval,'qr',sqr,istatus);ier=istatus+ier
    if (wrf_mass_regional) then
      call gsi_bundlegetpointer(sval,'qs',sqs,istatus);ier=istatus+ier
      call gsi_bundlegetpointer(sval,'qg',sqg,istatus);ier=istatus+ier
    end if

    call gsi_bundlegetpointer(rval,'qr',rqr,istatus);ier=istatus+ier
    if (wrf_mass_regional) then
      call gsi_bundlegetpointer(rval,'qs',rqs,istatus);ier=istatus+ier
      call gsi_bundlegetpointer(rval,'qg',rqg,istatus);ier=istatus+ier
    end if
  end if
  if(ier/=0)return

  dbzptr => dbzNode_typecast(dbzhead)
  do while (associated(dbzptr))
     if(dbzptr%luse)then
        if(nstep > 0)then
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
             valdbz= w1* rdbz(j1)+w2*rdbz(j2)+w3*rdbz(j3)+w4*rdbz(j4)+ &
                     w5* rdbz(j5)+w6*rdbz(j6)+w7*rdbz(j7)+w8*rdbz(j8)

             dbzcur= w1* sdbz(j1)+w2* sdbz(j2)+w3* sdbz(j3)+w4*sdbz(j4)+    &
                     w5* sdbz(j5)+w6* sdbz(j6)+w7* sdbz(j7)+w8* sdbz(j8)-   &
                     dbzptr%res

           else
             valqr=(w1* rqr(j1)+w2* rqr(j2)+w3* rqr(j3)+w4* rqr(j4)+       &
                    w5* rqr(j5)+w6* rqr(j6)+w7* rqr(j7)+w8* rqr(j8))
  
             qrcur=(w1* sqr(j1)+w2* sqr(j2)+w3* sqr(j3)+w4* sqr(j4)+       &
                    w5* sqr(j5)+w6* sqr(j6)+w7* sqr(j7)+w8* sqr(j8))

             if (wrf_mass_regional)then
               valqs=(w1* rqs(j1)+w2* rqs(j2)+w3* rqs(j3)+w4* rqs(j4)+ &
                      w5* rqs(j5)+w6* rqs(j6)+w7* rqs(j7)+w8* rqs(j8))

               qscur=(w1* sqs(j1)+w2* sqs(j2)+w3* sqs(j3)+w4* sqs(j4)+ &
                      w5* sqs(j5)+w6* sqs(j6)+w7* sqs(j7)+w8* sqs(j8))

               valqg=(w1* rqg(j1)+w2* rqg(j2)+w3* rqg(j3)+w4* rqg(j4)+ &
                      w5* rqg(j5)+w6* rqg(j6)+w7* rqg(j7)+w8* rqg(j8))

               qgcur=(w1* sqg(j1)+w2* sqg(j2)+w3* sqg(j3)+w4* sqg(j4)+ &
                      w5* sqg(j5)+w6* sqg(j6)+w7* sqg(j7)+w8* sqg(j8))

               valdbz = valqr * dbzptr%jqr + valqs *  dbzptr%jqs +     &
                        valqg * dbzptr%jqg
               if (l_use_dbz_caps) then !CAPS uses ddiff
                  dbzcur = qrcur * dbzptr%jqr + qscur * dbzptr%jqs +      &
                           qgcur * dbzptr%jqg - dbzptr%ddiff
               else
                  dbzcur = qrcur * dbzptr%jqr + qscur * dbzptr%jqs +      &
                           qgcur * dbzptr%jqg - dbzptr%res
               end if
             end if  

           end if


           do kk=1,nstep
              dbz=dbzcur+sges(kk)*valdbz            
              pen(kk)=dbz*dbz*dbzptr%err2
           end do
        else
           if (l_use_dbz_caps) then !CAPS uses ddiff
              pen(1)=dbzptr%ddiff*dbzptr%ddiff*dbzptr%err2
           else
              pen(1)=dbzptr%res*dbzptr%res*dbzptr%err2
           end if
        end if

!  Modify penalty term if nonlinear QC
        if (nlnqc_iter .and. dbzptr%pg > tiny_r_kind .and.  &
                             dbzptr%b  > tiny_r_kind) then
           write(6,*) 'No nonlinear QC for dbz now.'

           if (.not. l_use_dbz_caps ) then   ! CAPS blocked this
              pg_dbz=dbzptr%pg*varqc_iter                     
              cg_dbz=cg_term/dbzptr%b
              wnotgross= one-pg_dbz
              wgross = pg_dbz*cg_dbz/wnotgross
              do kk=1,max(1,nstep)
                 pen(kk)= -two*log((exp(-half*pen(kk)) + wgross)/(one+wgross))
              end do
           end if
        endif

        out(1) = out(1)+pen(1)*dbzptr%raterr2
        do kk=2,nstep
           out(kk) = out(kk)+(pen(kk)-pen(1))*dbzptr%raterr2
        end do

        if (.not. l_use_dbz_caps ) then   ! CAPS blocked this
           write(6,*)'stpdbz:   check penalty for dbz'
           do kk=1,nstep
              write(6,*)'step  penalty:',kk,out(kk)
           end do
        end if

     end if

     dbzptr => dbzNode_nextcast(dbzptr)

  end do
  return
end subroutine stpdbz

end module stpdbzmod
