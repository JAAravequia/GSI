!! the subroutine is to calculate the statistics

subroutine stascal(dtype,rdiag,nreal,n,iotype,varqc,ntype,work,worku,&
                   workv,np,ptop,pbot,nregion,mregion,&
                   rlatmin,rlatmax,rlonmin,rlonmax,iosubtype)

   implicit none

   real(4),dimension(nreal,n) :: rdiag
   real(4),dimension(100,2) :: varqc
   real(4),dimension(np) :: ptop,pbot
   real(4),dimension(mregion):: rlatmin,rlatmax,rlonmin,rlonmax

   !
   !  Here's the scoop on the dimensions for work:
   !
   !     np     = number of pressure bins
   !     100    = arbitrary limit to dtypes within a dtype (ie 120, 122, where
   !               dtype is someting like gps, t, q, etc) 
   !     6      = data type which are:
   !                 1 = count          obs count
   !                 2 = count_vqc      obs rejected by vqc
   !                 3 = bias (obs - ges)
   !                 4 = rms (root mean square)
   !                 5 = penalty
   !                 6 = qc penalty
   !     nreagion = number of geographic regions, 10 generally, and region 1 is
   !                global
   !     3      = obs classification (iclass) where:
   !                 1 = used
   !                 2 = rejected
   !                 3 = monitored
   ! 
   real(4),dimension(np,100,6,nregion,3) :: work,worku,workv
  
   !  These constant index values are designed to take some of the mystery out
   !  of life:
   integer, parameter  :: icount      = 1
   integer, parameter  :: icount_vqc  = 2
   integer, parameter  :: ibias       = 3
   integer, parameter  :: irms        = 4
   integer, parameter  :: ipen        = 5
   integer, parameter  :: iqc_pen     = 6

   integer, parameter  :: iused       = 1
   integer, parameter  :: ireject     = 2
   integer, parameter  :: imonitor    = 3

   integer, parameter  :: ibend       = 5
   integer, parameter  :: ierr1       = 14
   integer, parameter  :: ibendangobs = 17
   real, parameter     :: scale       = 100.0
   real, parameter     :: one         = 1.0

   integer  :: iclass

   character(3) :: dtype
  
   integer,dimension(100) :: iotype,iosubtype
   integer itype,isubtype,ilat,ilon,ipress,iqc,iuse,imuse
   integer iwgt,ierr2,ierr3,iobg,iobgu,iobgv,iqsges
   integer iobsu,iobsv,i,nregion,mregion,np,n,nreal,k,j
   integer ltype,ntype,intype,insubtype,nn
   real cg_term,pi,tiny
   real valu,valv,val,val2,gesu,gesv,spdb,exp_arg,arg
   real ress,ressu,ressv,valqc,term,wgross,cg_t,wnotgross
   real cvar_pg,cvar_b,rat_err2
   real ress_gps
   real data, rat_err

   !------------------------------------------------------
   !  These values need to be declared as parameters so
   !  they cannont be changed by accident:
   !------------------------------------------------------
   itype=1;isubtype=2;ilat=3;ilon=4;ipress=6;iqc=9;iuse=11;imuse=12
   iwgt=13;ierr2=15;ierr3=16;iobg=18;iobgu=18;iobgv=21;iqsges=20
   iobsu=17;iobsv=20

   !--------------------------------------------------------------------------------
   !  are any of these are available vi constant.f90?
   !  these declarations, as well as scale abovve, violate the principle of DRY.
   !--------------------------------------------------------------------------------
   pi=acos(-1.0)
   cg_term=sqrt(2.0*pi)/2.0
   tiny=1.0e-10

 
   print *,'--> stascal'

   do i=1,n
      if(trim(dtype) ==  ' uv') then
         valu=rdiag(iobgu,i)*rdiag(ierr1,i)
         valv=rdiag(iobgu,i)*rdiag(ierr1,i)
         val=0.5*(valu*valu+valv*valv)
         val2=val
         gesu=rdiag(iobgu,i)-rdiag(iobsu,i)
         gesv=rdiag(iobgv,i)-rdiag(iobsv,i)
         spdb=sqrt(rdiag(iobsu,i)**2+rdiag(iobsv,i)**2)-sqrt(gesu**2+gesv**2) 
      else
         val=rdiag(iobg,i)*rdiag(ierr1,i)
         val2     = val*val
      endif

      exp_arg  = -0.5*val2
      if ( rdiag(ierr3,i) <1.0e10 .and. rdiag(ierr1,i) <1.0e10 .and. rdiag(ierr1,i) >1.0e-10 ) then
         rat_err2 = (rdiag(ierr3,i)/rdiag(ierr1,i))**2
      else
         rat_err2 =0.0
      endif

    
      !-------------------------------------------------------------------------
      ! This is confusing. At least for gpsro data imuse is either 1 or -1 and
      ! ierr3 is always 0.00.  So this will not correctly assign data to the
      ! last dimension of work.  I'll override this in the gps section and come
      ! back to this to study other data.  My guess is that it's (now at least)
      ! wrong for other data -- it may never have been right.
      !
      ! Note further, the GrADS ctl file for this output file is formatted as
      ! used, rejected, monitored, which would logically associate the nn value
      ! with 1 = used, 2 = rej, 3 = monitor.  But this logic looks more like
      ! 1 = used, 2 = mon, 3 = reject.
      !
      ! Still further, setting nn=1 if imuse = 1 means that the count of used 
      ! obs includes all with an imuse = 1, then adds a count_vqc if they are
      ! rejected.  Whereas if it's monitored (imuse = -1), but then
      ! rdiag(ierr3,1) has a value, it counts as rejected, so it might not be
      ! possible for monitored dats to ever have a count_vqc.  Seems like there
      ! should be some consistency to this, so that all data counts as
      ! count(used) or count(montored) _and_ (as appropriate) count_vqc, _or_
      ! either count or count_vqc.  Right now it's different for used and
      ! monitored data, it would seem.
      !
      ! OK. On that last point the ctl file for all 3 categories of count 0:all,
      ! so I'm going to assume that the count includes both assim and rejected.
      !-------------------------------------------------------------------------
      if(rdiag(imuse,i) >0.0) then
         nn=1
      else
         nn=2
         if(rdiag(ierr3,i) >tiny) then
            nn=3
!            print *, 'NN=3 for trim(dtype) obs ', trim(dtype)
         endif
      endif


      do ltype=1,ntype

         intype=int(rdiag(itype,i))
         insubtype=int(rdiag(isubtype,i))
        
         
         if( trim(dtype) == 'gps' .and.  (intype == iotype(ltype)) ) then

            print *, 'Processing GPS, intype = ', intype

            !----------------------------------------------------------------------------
            !  NOTE this inconsistency in the rdiagbuf structure:
            !  for all obs types iqc is 9 but for gps it's 10.  Because reasons.
            !----------------------------------------------------------------------------
            iqc = 10


            do j=1,nregion

               if(rdiag(ilon,i) >180.0) then
                  rdiag(ilon,i)=rdiag(ilon,i)-360.0
               end if

               if(rdiag(ilon,i)>=rlonmin(j) .and. rdiag(ilon,i)<=rlonmax(j) .and. &
                  rdiag(ilat,i)>=rlatmin(j) .and. rdiag(ilat,i)<=rlatmax(j) ) then

                  do k=1,np
!                     print *, 'rdiag(ipress,i), ptop(k), pbot(k) = ', rdiag(ipress,i), ptop(k), pbot(k)


                     if(rdiag(ipress,i) >=ptop(k) .and. rdiag(ipress,i) <= pbot(k))then

                        if( rdiag(ipress,i) > 0.0 ) then
                           print *, 'rdiag(ipress,i), ptop(k), pbot(k),k = ', rdiag(ipress,i), ptop(k), pbot(k), k
                        end if

                        ress_gps = rdiag(ibend,i) * scale
!                        data     = one/rdiag(ierr1,i)
                        data     = rdiag(ierr1,i)
                        rat_err  = rdiag(ibendangobs,i)
                        val      = data * rat_err
                        val2     = val * val


                        !----------------------------------------------------------------------------
                        !  For gps data an imuse value of 1 is used, -1 is monitored, and an
                        !  iqc value > 0 is a rejection.  But there are stat fields for
                        !  count_vqc all classes of data (used, reject, monitor) so I think
                        !  that it has to be recorded first as either a class 1 or 3 (used,
                        !  monitor), then if the iqc flag is up it _also_ has to be included
                        !  in class reject.
                        !----------------------------------------------------------------------------
                        if(rdiag(imuse,i) > 0.0) then
                           iclass = iused
                           work(k,ltype,icount,j,iused) = work(k,ltype,icount,j,iused)+1.0
                           work(k,ltype,ibias,j,iused)  = work(k,ltype,ibias,j,iused) + ress_gps 
                           work(k,ltype,irms,j,iused)   = work(k,ltype,irms,j,iused) + ress_gps*ress_gps

                           !-------------------------------------------------------------------------
                           !  reverse engineering the penalty value, per genstats_gps.f90:
                           !            pen = val2*rat_err2
                           !  from setupbend.f90 we know that: 
                           !   1.  the error value, or data(ier,i) is one/rdiagbuf(14,i)
                           !   2.  dataerr is one/rdiagbuf(14,i) * rdiagbuf(17,i)
                           !   3.  rdiagbuf(14,i) = original invers obs error (rad**-1)
                           !   4.  rdiagbuf(17,i) = bending angle observation (rad)
                           !
                           !  So, the penalty, or val2 value is
                           !   val2 = (one/rdiagbuf(14,i) * rdiagbuf(17,i) ) **2
                           !   values for 14 and 17 are defined respectively as local parameters 
                           !        ierr1, ibendangobs

                           print *, 'PEN components:  rdiag(ierr1,i), data, rat_err, val, val2 = ', rdiag(ierr1,i), data, rat_err, val, val2

!                           work(k,ltype,ipen,j,iused) = work(k,ltype,ipen,j,iused) + val2
                           work(k,ltype,ipen,j,iused) = work(k,ltype,ipen,j,iused) + val

                        else if( rdiag(imuse,i) < 0.0 ) then
                           iclass = imonitor
                        end if


                        if( iclass /= iused ) then
                           print *, ' NOT USED:  i, rdiag(iuse,i), rdiag(imuse,i), rdiag(iwgt,i)  = ', i, rdiag(iuse,i), rdiag(imuse,i), rdiag(iwgt,i)
                        else
                           print *, ' IUSED:  i, rdiag(iuse,i), rdiag(imuse,i), rdiag(iwgt,i) = ', i, rdiag(iuse,i), rdiag(imuse,i), rdiag(iwgt,i)
                        end if

                        if(rdiag(iwgt,i) <1.0) then
                           print *, ' LOW IWGT, i rdiag(iwgt,i) = ', i, rdiag(iwgt, i)
                        end if

                        !-------------------------------------------------------------------
                        !  Based on how the other data types are handled the
                        !  intent is to count all obs as either used or
                        !  monitored, then add them to the class of count_vqc
                        !  and type rejected.  
                        !
                        !  Update -- counting when imuse == 1 gets a count that
                        !  matches the results in the gsistat file.
                        !-------------------------------------------------------------------


                        !--------------------------------------------------
                        ! For gps an rdiag(iqc,i) value of 
                        !     0.0         = good 
                        !     1.0,2.0,4.0 = rejected by qc
                        !     3.0         = rejected by gross check.
                        !
                        ! But here's what I'm using because it makes the stats
                        ! agree with the gsistat output:
                        !     0.0         = good 
                        !     2.0,4.0     = rejected by qc
                        !     3.0         = rejected by gross check.
                        !     1.0         = ignored
                        !     
                        ! This is complicated.  If obs is rejected:
                        !   1.  obs is added to work as either use or mon,
                        !          for both count and count_iqc, then
                        !   2.  change iclass=ireject and do the same
                        !
                        ! I'm not sure why there are different data types for
                        ! count and count_vqc when the obs class also includes
                        ! rejected by vqc but there it is.  I just want to make
                        ! sure the numbers are correct, even if they are
                        ! duplicates.
                        !--------------------------------------------------
                        if( rdiag(iqc,i) == 2.0 .or. rdiag(iqc,i) == 4.0 ) then

                           work(k,ltype,icount,j,ireject)     = work(k,ltype,icount,j,ireject)+1.0

                           work(k,ltype,icount_vqc,j,icount)  = work(k,ltype,icount_vqc,j,icount)+1.0
                           work(k,ltype,icount_vqc,j,ireject) = work(k,ltype,icount_vqc,j,ireject)+1.0
                           work(k,ltype,ibias,j,ireject)      = work(k,ltype,ibias,j,ireject) + ress_gps 
                           work(k,ltype,irms,j,ireject)       = work(k,ltype,irms,j,ireject) + ress_gps*ress_gps
                           work(k,ltype,ipen,j,ireject)       = work(k,ltype,ipen,j,ireject) + val


!                           print *, 'k, ltype, icount, icount_vqc, j, iclass, work(k,ltype,icount,j,iclass), work(k,ltype,icount_vqc,j,iclass) = ', &
!                                     k, ltype, icount, icount_vqc, j, iclass, work(k,ltype,icount,j,iclass), work(k,ltype,icount_vqc,j,iclass) 

!                           print *, '   setting iclass = ireject'

!                           iclass = ireject
!                           work(k,ltype,icount,j,iclass)=work(k,ltype,icount,j,iclass)+1.0
!
!                           print *, '   work(k,ltype,icount,j,iclass) = ', k, ltype, icount, j, iclass, work(k,ltype,icount,j,iclass) 
!
!                           work(k,ltype,icount_vqc,j,iclass)=work(k,ltype,icount_vqc,j,iclass)+1.0
!
!                           print *, 'k, ltype, icount, icount_vqc, j, iclass, work(k,ltype,icount,j,iclass), work(k,ltype,icount_vqc,j,iclass) = ', &
!                                     k, ltype, icount, icount_vqc, j, iclass, work(k,ltype,icount,j,iclass), work(k,ltype,icount_vqc,j,iclass) 

!                        else
!                           print *, 'NOT A REJECT'
!                           print *, 'k, ltype, icount, icount_vqc, j, iclass, work(k,ltype,icount,j,iclass), work(k,ltype,icount_vqc,j,iclass) = ', &
!                                     k, ltype, icount, icount_vqc, j, iclass, work(k,ltype,icount,j,iclass), work(k,ltype,icount_vqc,j,iclass) 
                        end if

                     end if

                  end do  !!! k
               end if     !!! rdiag(ilon,i)
            end do        !!! region

         else if( (intype == iotype(ltype)) .and. (insubtype == iosubtype(ltype)) ) then
             

            cvar_pg=varqc(ltype,2)
            cvar_b=varqc(ltype,1)
            if (cvar_pg > 0.0 .and. rdiag(imuse,i) >0.0) then
               arg  = exp(exp_arg)
               wnotgross= 1.0-cvar_pg
               cg_t=cvar_b
               wgross = cg_term*cvar_pg/(cg_t*wnotgross)
               term =log((arg+wgross)/(1.0+wgross))
            else
               term = exp_arg
            endif
            valqc = -2.0*rat_err2*term


            do j=1,nregion

               if(rdiag(ilon,i) >180.0) then
                  rdiag(ilon,i)=rdiag(ilon,i)-360.0
               end if

               if(rdiag(ilon,i)>=rlonmin(j) .and. rdiag(ilon,i)<=rlonmax(j) .and. &
                  rdiag(ilat,i)>=rlatmin(j) .and. rdiag(ilat,i)<=rlatmax(j) ) then

                  do k=1,np
                     if(rdiag(ipress,i) >=ptop(k) .and. rdiag(ipress,i) <= pbot(k))then

                        !-------------------------------------------------------------
                        ! count, this is counting all obs w/in the pressure bin
                        ! for the type nn
                        !-------------------------------------------------------------
                        work(k,ltype,1,j,nn)=work(k,ltype,1,j,nn)+1.0

                        ! 2 = count_vqc
                        if(rdiag(iwgt,i) <1.0) then
                           work(k,ltype,2,j,nn)=work(k,ltype,2,j,nn)+1.0
                        end if

                        if(trim(dtype) == '  q') then
                           ress=rdiag(iobg,i)*100.0/rdiag(iqsges,i)
                           work(k,ltype,3,j,nn)=work(k,ltype,3,j,nn)+ress
                        else if (trim(dtype) == ' uv') then
                           ress=sqrt(rdiag(iobgu,i)**2+rdiag(iobgv,i)**2)
                           ressu=rdiag(iobgu,i)
                           ressv=rdiag(iobgv,i)
                           work(k,ltype,3,j,nn)=work(k,ltype,3,j,nn)+spdb
                           worku(k,ltype,3,j,nn)=worku(k,ltype,3,j,nn)+ressu
                           workv(k,ltype,3,j,nn)=workv(k,ltype,3,j,nn)+ressv
                           worku(k,ltype,4,j,nn)=worku(k,ltype,4,j,nn)+ressu*ressu
                           workv(k,ltype,4,j,nn)=workv(k,ltype,4,j,nn)+ressv*ressv
                        else
                           ress=rdiag(iobg,i)
                           work(k,ltype,3,j,nn)=work(k,ltype,3,j,nn)+ress
                        endif

                        work(k,ltype,4,j,nn)=work(k,ltype,4,j,nn)+ress*ress

                        work(k,ltype,5,j,nn)=work(k,ltype,5,j,nn)+val2*rat_err2
                        work(k,ltype,6,j,nn)=work(k,ltype,6,j,nn)+valqc

                     endif       !!! endif if(rdiag(ipress,i)
                  enddo        !!! enddo k
               endif     !!! endif region

            enddo         !!! enddo region
         
         endif        !!! endif ltype

      enddo         !!  enddo ltype

   enddo            !!! enddo i


!!   if( trim(dtype) == 'gps' .and.  (intype == iotype(ltype)) ) then
!      print *, 'work(1,1,1,1,1), work(1,1,2,1,1) = ', work(1,1,1,1,1), work(1,1,2,1,1)
!      print *, 'work(1,1,1,1,2), work(1,1,2,1,2) = ', work(1,1,1,1,2), work(1,1,2,1,2)
!      print *, 'work(1,1,1,1,3), work(1,1,2,1,3) = ', work(1,1,1,1,3), work(1,1,2,1,3)
!!   endif

   print *, '<-- stascal'
   return
end
