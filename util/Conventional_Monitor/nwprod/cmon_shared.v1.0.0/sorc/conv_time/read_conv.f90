!   intype  : the observarion type like t for tem., uv for wind
!   stype   : the observation sub type, like t120 uv220
!   *work   : the arrays hold statistics for each obs type: 
!               1st variable = vertical level 
!               2nd variable = is the number of data types
!               3rd variable = statistics variable where: 
!                       1 = count 
!                       2 = count rejected by variational qc
!                       3 = bias
!                       4 = rms 
!                       5 = penalty
!                       6 = variational penalty 
!               4th variable = region 
!               5th variable = data usuage type where:
!                       1 = used 
!                       2 = rejected 
!                       3 = monited


subroutine read_conv(filein,mregion,nregion,np,ptop,pbot,ptopq,pbotq,&
           rlatmin,rlatmax,rlonmin,rlonmax,iotype_gps,iotype_ps,iotype_q,&
           iotype_t,iotype_uv,varqc_gps,varqc_ps,varqc_q,varqc_t,varqc_uv,&
           ntype_gps,ntype_ps,ntype_q,ntype_t,ntype_uv,&
           iosubtype_gps,iosubtype_ps,iosubtype_q,iosubtype_t,iosubtype_uv)


   implicit none

   integer mregion
   real(4),allocatable,dimension(:,:)  :: rdiag 
   character(8),allocatable,dimension(:)  :: cdiag 
   real(4),dimension(np) :: ptop,pbot,ptopq,pbotq
   real(4),dimension(np,100,6,nregion,3) :: twork,qwork,uwork,vwork,uvwork
   real(4),dimension(np,100,6,nregion,3) :: gpswork
   real(4),dimension(1,100,6,nregion,3) :: pswork
   real,dimension(mregion):: rlatmin,rlatmax,rlonmin,rlonmax

   integer,dimension(100) :: iotype_gps,iotype_ps,iotype_q,iotype_t,iotype_uv
   integer,dimension(100) :: iosubtype_gps,iosubtype_ps,iosubtype_q,iosubtype_uv,iosubtype_t
   real(4),dimension(100,2) :: varqc_gps,varqc_ps,varqc_q,varqc_t,varqc_uv
   character(20) :: filein
   character(3) :: dtype

   integer nchar,nreal,ii,mype,idate,iflag,itype
   integer lunin,lunot,nreal1,nreal2,ldtype,intype
   integer ilat,ilon,ipress,iqc,iuse,imuse,iwgt,ierr1
   integer ierr2,ierr3,ipsobs,iqobs,ioff02
   integer i,j,k,np,nregion,ltype,iregion,ntype_uv
   integer iobg,iobgu,iobgv,ntype_gps,ntype_ps,ntype_q,ntype_t
   integer iclass

   real(4) ::  bmiss
   
   data lunin / 11 /
   data lunot / 21 /
   data bmiss /-999.0/



   print *, '--> read_conv'
   twork=0.0;qwork=0.0;uwork=0.0;vwork=0.0;uvwork=0.0;gpswork=0.0
   pswork=0.0

   itype=1;ilat=3;ilon=4;ipress=6;iqc=9;iuse=11;imuse=12
   iwgt=13;ierr1=14;ierr2=15;ierr3=16;iobg=18;iobgu=18;iobgv=21
   

   open(lunin,file=filein,form='unformatted')  
   rewind(lunin)

   read(lunin) idate

   print *, 'idate=',idate 
   print *,'ptop(1), ptop(5) = ', ptop(1),ptop(5)
   print *,'pbot(1), pbot(5) = ', pbot(1),pbot(5)
   print *,'ntype_gps = ', ntype_gps

   loopd: do  
      read(lunin,IOSTAT=iflag) dtype,nchar,nreal,ii,mype,ioff02
      if( iflag /= 0 ) exit loopd
      print *, 'dtype, nchar, nreal, ii, mpye = ', dtype,nchar,nreal,ii,mype

      allocate(cdiag(ii),rdiag(nreal,ii))
      read(lunin,IOSTAT=iflag) cdiag,rdiag

      if( iflag /= 0 ) exit loopd


      if(trim(dtype) == 'gps') then
         call stascal(dtype,rdiag,nreal,ii,iotype_gps,varqc_gps,ntype_gps,&
                         gpswork,uwork,vwork,np,ptop,pbot,nregion,mregion,&
                         rlatmin,rlatmax,rlonmin,rlonmax,iosubtype_gps)

      else if(trim(dtype) == ' ps') then
         call stascal(dtype,rdiag,nreal,ii,iotype_ps,varqc_ps,ntype_ps,&
                         pswork,uwork,vwork,1,ptop,pbot,nregion,mregion,&
                         rlatmin,rlatmax,rlonmin,rlonmax,iosubtype_ps)

      else if(trim(dtype) == '  q') then
         call stascal(dtype,rdiag,nreal,ii,iotype_q,varqc_q,ntype_q,&
                         qwork,uwork,vwork,np,ptopq,pbotq,nregion,mregion,&
                         rlatmin,rlatmax,rlonmin,rlonmax,iosubtype_q)

      else if(trim(dtype) == '  t') then
         call stascal(dtype,rdiag,nreal,ii,iotype_t,varqc_t,ntype_t,&
                         twork,uwork,vwork,np,ptop,pbot,nregion,mregion,&
                         rlatmin,rlatmax,rlonmin,rlonmax,iosubtype_t)
         
      else if(trim(dtype) == ' uv') then
         call stascal(dtype,rdiag,nreal,ii,iotype_uv,varqc_uv,ntype_uv,&
                         uvwork,uwork,vwork,np,ptop,pbot,nregion,mregion,&
                         rlatmin,rlatmax,rlonmin,rlonmax,iosubtype_uv)
         
      endif
          
      deallocate(cdiag,rdiag)
   enddo   loopd               !  ending read data do loop

   close(lunin)


   do iregion=1,nregion
      do j=1,3
         do ltype=1,ntype_ps
            pswork(1,ntype_ps+1,1,iregion,j)= &
                  pswork(1,ntype_ps+1,1,iregion,j)+pswork(1,ltype,1,iregion,j)
            pswork(1,ntype_ps+1,2,iregion,j)= &
                  pswork(1,ntype_ps+1,2,iregion,j)+pswork(1,ltype,2,iregion,j)
            pswork(1,ntype_ps+1,3,iregion,j)= &
                  pswork(1,ntype_ps+1,3,iregion,j)+pswork(1,ltype,3,iregion,j)
            pswork(1,ntype_ps+1,4,iregion,j)= &
                  pswork(1,ntype_ps+1,4,iregion,j)+pswork(1,ltype,4,iregion,j)
            pswork(1,ntype_ps+1,5,iregion,j)= &
                  pswork(1,ntype_ps+1,5,iregion,j)+pswork(1,ltype,5,iregion,j)
            pswork(1,ntype_ps+1,6,iregion,j)= &
                  pswork(1,ntype_ps+1,6,iregion,j)+pswork(1,ltype,6,iregion,j)

            if(pswork(1,ltype,1,iregion,j) >=1.0) then
               pswork(1,ltype,3,iregion,j)= &
                     pswork(1,ltype,3,iregion,j)/pswork(1,ltype,1,iregion,j)
               pswork(1,ltype,4,iregion,j)= &
                     sqrt(pswork(1,ltype,4,iregion,j)/pswork(1,ltype,1,iregion,j))
               pswork(1,ltype,5,iregion,j)= &
                     pswork(1,ltype,5,iregion,j)/pswork(1,ltype,1,iregion,j)
               pswork(1,ltype,6,iregion,j)= &
                     pswork(1,ltype,6,iregion,j)/pswork(1,ltype,1,iregion,j)
            endif
         enddo

         !----------------------------------------------
         ! for the total surface pressure statistics
         !
         if(pswork(1,ntype_ps+1,1,iregion,j) >=1.0) then
            pswork(1,ntype_ps+1,3,iregion,j) = pswork(1,ntype_ps+1,3,iregion,j)/&
                                    pswork(1,ntype_ps+1,1,iregion,j)
            pswork(1,ntype_ps+1,4,iregion,j) = sqrt(pswork(1,ntype_ps+1,4,iregion,j)&
                                    /pswork(1,ntype_ps+1,1,iregion,j))
            pswork(1,ntype_ps+1,5,iregion,j) = pswork(1,ntype_ps+1,5,iregion,j)/&
                                    pswork(1,ntype_ps+1,1,iregion,j)
            pswork(1,ntype_ps+1,6,iregion,j) = pswork(1,ntype_ps+1,6,iregion,j)/&
                                    pswork(1,ntype_ps+1,1,iregion,j)
         endif
                                    
         do k=1,np
            do ltype=1,ntype_q
               qwork(k,ntype_q+1,1,iregion,j) = &
                        qwork(k,ntype_q+1,1,iregion,j)+qwork(k,ltype,1,iregion,j)
               qwork(k,ntype_q+1,2,iregion,j) = &
                        qwork(k,ntype_q+1,2,iregion,j)+qwork(k,ltype,2,iregion,j)
               qwork(k,ntype_q+1,3,iregion,j) = &
                        qwork(k,ntype_q+1,3,iregion,j)+qwork(k,ltype,3,iregion,j)
               qwork(k,ntype_q+1,4,iregion,j) = &
                        qwork(k,ntype_q+1,4,iregion,j)+qwork(k,ltype,4,iregion,j)
               qwork(k,ntype_q+1,5,iregion,j) = &
                        qwork(k,ntype_q+1,5,iregion,j)+qwork(k,ltype,5,iregion,j)
               qwork(k,ntype_q+1,6,iregion,j) = &
                        qwork(k,ntype_q+1,6,iregion,j)+qwork(k,ltype,6,iregion,j)

               if(qwork(k,ltype,1,iregion,j) >=1.0) then
                  qwork(k,ltype,3,iregion,j) = &
                        qwork(k,ltype,3,iregion,j)/qwork(k,ltype,1,iregion,j)
                  qwork(k,ltype,4,iregion,j) = &
                        sqrt(qwork(k,ltype,4,iregion,j)/qwork(k,ltype,1,iregion,j))
                  qwork(k,ltype,5,iregion,j) = &
                        qwork(k,ltype,5,iregion,j)/qwork(k,ltype,1,iregion,j)
                  qwork(k,ltype,6,iregion,j) = &
                        qwork(k,ltype,6,iregion,j)/qwork(k,ltype,1,iregion,j)
               endif
            enddo

            if(qwork(k,ntype_q+1,1,iregion,j) >=1.0) then
               qwork(k,ntype_q+1,3,iregion,j)=qwork(k,ntype_q+1,3,iregion,j)/&
                                 qwork(k,ntype_q+1,1,iregion,j)
               qwork(k,ntype_q+1,4,iregion,j)=sqrt(qwork(k,ntype_q+1,4,iregion,j)/&
                                 qwork(k,ntype_q+1,1,iregion,j))
               qwork(k,ntype_q+1,5,iregion,j)=qwork(k,ntype_q+1,5,iregion,j)/&
                                 qwork(k,ntype_q+1,1,iregion,j)
               qwork(k,ntype_q+1,6,iregion,j)=qwork(k,ntype_q+1,6,iregion,j)/&
                                 qwork(k,ntype_q+1,1,iregion,j)
            endif

            do ltype=1,ntype_t
               twork(k,ntype_t+1,1,iregion,j) = &
                        twork(k,ntype_t+1,1,iregion,j)+twork(k,ltype,1,iregion,j)
               twork(k,ntype_t+1,2,iregion,j) = &
                        twork(k,ntype_t+1,2,iregion,j)+twork(k,ltype,2,iregion,j)
               twork(k,ntype_t+1,3,iregion,j) = &
                        twork(k,ntype_t+1,3,iregion,j)+twork(k,ltype,3,iregion,j)
               twork(k,ntype_t+1,4,iregion,j) = &
                        twork(k,ntype_t+1,4,iregion,j)+twork(k,ltype,4,iregion,j)
               twork(k,ntype_t+1,5,iregion,j) = &
                        twork(k,ntype_t+1,5,iregion,j)+twork(k,ltype,5,iregion,j)
               twork(k,ntype_t+1,6,iregion,j) = &
                        twork(k,ntype_t+1,6,iregion,j)+twork(k,ltype,6,iregion,j)

               if(twork(k,ltype,1,iregion,j) >=1.0) then
                  twork(k,ltype,3,iregion,j) = &
                        twork(k,ltype,3,iregion,j)/twork(k,ltype,1,iregion,j)
                  twork(k,ltype,4,iregion,j) = &
                        sqrt(twork(k,ltype,4,iregion,j)/twork(k,ltype,1,iregion,j))
                  twork(k,ltype,5,iregion,j) = &
                        twork(k,ltype,5,iregion,j)/twork(k,ltype,1,iregion,j)
                  twork(k,ltype,6,iregion,j) = &
                        twork(k,ltype,6,iregion,j)/twork(k,ltype,1,iregion,j)
               endif
            enddo

            if(twork(k,ntype_t+1,1,iregion,j) >=1.0) then
               twork(k,ntype_t+1,3,iregion,j) = twork(k,ntype_t+1,3,iregion,j)/&
                                 twork(k,ntype_t+1,1,iregion,j)
               twork(k,ntype_t+1,4,iregion,j)=sqrt(twork(k,ntype_t+1,4,iregion,j)/&
                                 twork(k,ntype_t+1,1,iregion,j))
               twork(k,ntype_t+1,5,iregion,j)=twork(k,ntype_t+1,5,iregion,j)/&
                                 twork(k,ntype_t+1,1,iregion,j)
               twork(k,ntype_t+1,6,iregion,j)=twork(k,ntype_t+1,6,iregion,j)/&
                                 twork(k,ntype_t+1,1,iregion,j)
            endif

            do ltype=1,ntype_uv
               uvwork(k,ntype_uv+1,1,iregion,j) = &
                        uvwork(k,ntype_uv+1,1,iregion,j)+uvwork(k,ltype,1,iregion,j)
               uvwork(k,ntype_uv+1,2,iregion,j) = &
                        uvwork(k,ntype_uv+1,2,iregion,j)+uvwork(k,ltype,2,iregion,j)
               uvwork(k,ntype_uv+1,3,iregion,j) = &
                        uvwork(k,ntype_uv+1,3,iregion,j)+uvwork(k,ltype,3,iregion,j)
               uvwork(k,ntype_uv+1,4,iregion,j) = &
                        uvwork(k,ntype_uv+1,4,iregion,j)+uvwork(k,ltype,4,iregion,j)
               uvwork(k,ntype_uv+1,5,iregion,j) = &
                        uvwork(k,ntype_uv+1,5,iregion,j)+uvwork(k,ltype,5,iregion,j)
               uvwork(k,ntype_uv+1,6,iregion,j) = &
                        uvwork(k,ntype_uv+1,6,iregion,j)+uvwork(k,ltype,6,iregion,j)
               uwork(k,ntype_uv+1,3,iregion,j) = &
                        uwork(k,ntype_uv+1,3,iregion,j)+uwork(k,ltype,3,iregion,j)
               uwork(k,ntype_uv+1,4,iregion,j) = &
                        uwork(k,ntype_uv+1,4,iregion,j)+uwork(k,ltype,4,iregion,j)
               vwork(k,ntype_uv+1,3,iregion,j) = &
                        vwork(k,ntype_uv+1,3,iregion,j)+vwork(k,ltype,3,iregion,j)
               vwork(k,ntype_uv+1,4,iregion,j) = &
                        vwork(k,ntype_uv+1,4,iregion,j)+vwork(k,ltype,4,iregion,j)

               if(uvwork(k,ltype,1,iregion,j) >=1.0) then
                  uvwork(k,ltype,3,iregion,j) = &
                        uvwork(k,ltype,3,iregion,j)/uvwork(k,ltype,1,iregion,j)
                  uvwork(k,ltype,4,iregion,j) = &
                        sqrt(uvwork(k,ltype,4,iregion,j)/uvwork(k,ltype,1,iregion,j))
                  uvwork(k,ltype,5,iregion,j) = &
                        uvwork(k,ltype,5,iregion,j)/uvwork(k,ltype,1,iregion,j)
                  uvwork(k,ltype,6,iregion,j) = &
                        uvwork(k,ltype,6,iregion,j)/uvwork(k,ltype,1,iregion,j)
                  uwork(k,ltype,1,iregion,j) = uvwork(k,ltype,1,iregion,j)
                  vwork(k,ltype,1,iregion,j) = uvwork(k,ltype,1,iregion,j)
                  uwork(k,ltype,2,iregion,j) = uvwork(k,ltype,2,iregion,j)
                  vwork(k,ltype,2,iregion,j) = uvwork(k,ltype,2,iregion,j)
                  uwork(k,ltype,3,iregion,j) = &
                        uwork(k,ltype,3,iregion,j)/uvwork(k,ltype,1,iregion,j)
                  uwork(k,ltype,4,iregion,j) = &
                        sqrt(uwork(k,ltype,4,iregion,j)/uvwork(k,ltype,1,iregion,j))
                  vwork(k,ltype,3,iregion,j) = &
                        vwork(k,ltype,3,iregion,j)/uvwork(k,ltype,1,iregion,j)
                  vwork(k,ltype,4,iregion,j) = &
                        sqrt(vwork(k,ltype,4,iregion,j)/uvwork(k,ltype,1,iregion,j))
               endif
            enddo

            if(uvwork(k,ntype_uv+1,1,iregion,j) >=1.0) then
               uvwork(k,ntype_uv+1,3,iregion,j)=uvwork(k,ntype_uv+1,3,iregion,j)&
                                  /uvwork(k,ntype_uv+1,1,iregion,j)
               uvwork(k,ntype_uv+1,4,iregion,j)=sqrt(uvwork(k,ntype_uv+1,4,iregion,j)&
                                  /uvwork(k,ntype_uv+1,1,iregion,j))
               uvwork(k,ntype_uv+1,5,iregion,j)=uvwork(k,ntype_uv+1,5,iregion,j)&
                                  /uvwork(k,ntype_uv+1,1,iregion,j)
               uvwork(k,ntype_uv+1,6,iregion,j)=uvwork(k,ntype_uv+1,6,iregion,j)&
                                  /uvwork(k,ntype_uv+1,1,iregion,j)
               uwork(k,ntype_uv+1,1,iregion,j)=uvwork(k,ntype_uv+1,1,iregion,j)
               uwork(k,ntype_uv+1,2,iregion,j)=uvwork(k,ntype_uv+1,2,iregion,j)
               vwork(k,ntype_uv+1,1,iregion,j)=uvwork(k,ntype_uv+1,1,iregion,j)
               vwork(k,ntype_uv+1,2,iregion,j)=uvwork(k,ntype_uv+1,2,iregion,j)

               uwork(k,ntype_uv+1,3,iregion,j)=uwork(k,ntype_uv+1,3,iregion,j)&
                                  /uwork(k,ntype_uv+1,1,iregion,j)
               uwork(k,ntype_uv+1,4,iregion,j)=sqrt(uwork(k,ntype_uv+1,4,iregion,j)&
                                  /uwork(k,ntype_uv+1,1,iregion,j))
               vwork(k,ntype_uv+1,3,iregion,j)=vwork(k,ntype_uv+1,3,iregion,j)&
                                  /vwork(k,ntype_uv+1,1,iregion,j)
               vwork(k,ntype_uv+1,4,iregion,j)=sqrt(vwork(k,ntype_uv+1,4,iregion,j)&
                                  /vwork(k,ntype_uv+1,1,iregion,j))
            endif
   
!            do ltype=1,ntype_gps
!               gpswork(k,ntype_gps+1,1,iregion,j) = &
!                        gpswork(k,ntype_gps+1,1,iregion,j)+gpswork(k,ltype,1,iregion,j)
!               gpswork(k,ntype_gps+1,2,iregion,j) = &
!                        gpswork(k,ntype_gps+1,2,iregion,j)+gpswork(k,ltype,2,iregion,j)
!               gpswork(k,ntype_gps+1,3,iregion,j) = &
!                        gpswork(k,ntype_gps+1,3,iregion,j)+gpswork(k,ltype,3,iregion,j)
!               gpswork(k,ntype_gps+1,4,iregion,j) = &
!                        gpswork(k,ntype_gps+1,4,iregion,j)+gpswork(k,ltype,4,iregion,j)
!
!               gpswork(k,ntype_gps+1,5,iregion,j) = &
!                        gpswork(k,ntype_gps+1,5,iregion,j)+gpswork(k,ltype,5,iregion,j)
!               gpswork(k,ntype_gps+1,6,iregion,j) = &
!                        gpswork(k,ntype_gps+1,6,iregion,j)+gpswork(k,ltype,6,iregion,j)
!
!               if(gpswork(k,ltype,1,iregion,j) >=1.0) then
!                  gpswork(k,ltype,3,iregion,j) = &
!                        gpswork(k,ltype,3,iregion,j)/gpswork(k,ltype,1,iregion,j)
!                  gpswork(k,ltype,4,iregion,j) = &
!                        sqrt(gpswork(k,ltype,4,iregion,j)/gpswork(k,ltype,1,iregion,j))
!                  gpswork(k,ltype,5,iregion,j) = &
!                        gpswork(k,ltype,5,iregion,j)/gpswork(k,ltype,1,iregion,j)
!                  gpswork(k,ltype,6,iregion,j) = &
!                        gpswork(k,ltype,6,iregion,j)/gpswork(k,ltype,1,iregion,j)
!               endif
!            enddo

!            if(gpswork(k,ntype_gps+1,1,iregion,j) >=1.0) then
!               gpswork(k,ntype_gps+1,3,iregion,j) = gpswork(k,ntype_gps+1,3,iregion,j)/&
!                                 gpswork(k,ntype_gps+1,1,iregion,j)
!               gpswork(k,ntype_gps+1,4,iregion,j)=sqrt(gpswork(k,ntype_gps+1,4,iregion,j)/&
!                                 gpswork(k,ntype_gps+1,1,iregion,j))
!               gpswork(k,ntype_gps+1,5,iregion,j)=gpswork(k,ntype_gps+1,5,iregion,j)/&
!                                 gpswork(k,ntype_gps+1,1,iregion,j)
!               gpswork(k,ntype_gps+1,6,iregion,j)=gpswork(k,ntype_gps+1,6,iregion,j)/&
!                                 gpswork(k,ntype_gps+1,1,iregion,j)
!            endif

         enddo    !!! enddo k height
      enddo       !!! enddo j, j=1 assimilated, j=2 rejected, j=3 monitored 
   enddo          !!! enddo iregion region 


   print *, 'end of diag2grad subroutine'


   !!! open the grads output files

   !!! for surface pressure files
   open(21,file='ps_stas',form='unformatted')    
   do j=1,3
      do i=1,6
         write(21) ((pswork(1,ltype,i,iregion,j),ltype=1,ntype_ps+1),iregion=1,nregion)
      enddo
   enddo
   close(21)

   open(31,file='q_stas',form='unformatted')
   do j=1,3
      do i=1,6
         do k=1,np
            write(31) ((qwork(k,ltype,i,iregion,j),ltype=1,ntype_q+1),iregion=1,nregion)
         enddo
      enddo
   enddo
   close(31)

   open(41,file='t_stas',form='unformatted')
   do j=1,3
      do i=1,6
         do k=1,np
            write(41) ((twork(k,ltype,i,iregion,j),ltype=1,ntype_t+1),iregion=1,nregion)
         enddo
      enddo
   enddo

   900 format(13f8.2)
   close(41)

   open(51,file='u_stas',form='unformatted')
   do j=1,3
      do i=1,6
         do k=1,np
            write(51) ((uwork(k,ltype,i,iregion,j),ltype=1,ntype_uv+1),iregion=1,nregion)
         enddo
      enddo
   enddo
   close(51)

   open(61,file='v_stas',form='unformatted')
   do j=1,3
      do i=1,6
         do k=1,np
            write(61) ((vwork(k,ltype,i,iregion,j),ltype=1,ntype_uv+1),iregion=1,nregion)
         enddo
      enddo
   enddo
   close(61)
     
   open(71,file='uv_stas',form='unformatted')
   do j=1,3
      do i=1,6
         do k=1,np
            write(71) ((uvwork(k,ltype,i,iregion,j),ltype=1,ntype_uv+1), &
                                iregion=1,nregion)
         enddo
      enddo
   enddo
   close(71)

   open(81,file='gps_stas',form='unformatted')
   do j=1,3
      do i=1,6
         do k=1,np
            write(81) ((gpswork(k,ltype,i,iregion,j),ltype=1,ntype_gps+1),iregion=1,nregion)
         enddo
      enddo
   enddo

   print *, 'gpswork RESULTS:'
   print *, ' '
   print *, '    np = ', np
   print *, ' '
!   write(6,900) ((gpswork(k,1,1,1,iclass),k=1,np),iclass=1,3) 
!   do k=1,3
      print *, '   iotype_gps = ', iotype_gps
      k=1

!      write(6,900) ((gpswork(k,ltype,1,1,iclass),ltype=1,ntype_gps+1),iclass=1,3) 

       print *, ' '
       print *, 'case gps, at end :'
       print *, ' count: '
       print *, 'gpswork(1,1,1,1,1) = ', gpswork(1,1,1,1,1)
       print *, 'gpswork(1,1,1,1,2) = ', gpswork(1,1,1,1,2)
       print *, 'gpswork(1,1,1,1,3) = ', gpswork(1,1,1,1,3)
       print *, ' count_vqc: '
       print *, 'gpswork(1,1,2,1,1) = ', gpswork(1,1,2,1,1)
       print *, 'gpswork(1,1,2,1,2) = ', gpswork(1,1,2,1,2)
       print *, 'gpswork(1,1,2,1,3) = ', gpswork(1,1,2,1,3)
       print *, ' bias: '
       print *, 'gpswork(1,1,3,1,1) = ', gpswork(1,1,3,1,1)
       print *, 'gpswork(1,1,3,1,2) = ', gpswork(1,1,3,1,2)
       print *, 'gpswork(1,1,3,1,3) = ', gpswork(1,1,3,1,3)
       print *, ' rms: '
       print *, 'gpswork(1,1,4,1,1) = ', gpswork(1,1,4,1,1)
       print *, 'gpswork(1,1,4,1,2) = ', gpswork(1,1,4,1,2)
       print *, 'gpswork(1,1,4,1,3) = ', gpswork(1,1,4,1,3)
       print *, ' pen: '
       print *, 'gpswork(1,1,5,1,1) = ', gpswork(1,1,5,1,1)
       print *, 'gpswork(1,1,5,1,2) = ', gpswork(1,1,5,1,2)
       print *, 'gpswork(1,1,5,1,3) = ', gpswork(1,1,5,1,3)
       print *, ' qc_pen: '
       print *, 'gpswork(1,1,6,1,1) = ', gpswork(1,1,6,1,1)
       print *, 'gpswork(1,1,6,1,2) = ', gpswork(1,1,6,1,2)
       print *, 'gpswork(1,1,6,1,3) = ', gpswork(1,1,6,1,3)

       print *, ' '
       print *, '  gps counts, all pressure bins, dtype 1, region 1, used :'
       write(6,900) (gpswork(k,1,1,1,1),k=1,np) 
       print *, ' '
       print *, '  gps counts rej qc, all pressure bins, dtype 1, region 1, used :'
       write(6,900) (gpswork(k,1,2,1,1),k=1,np) 
       print *, ' '
       print *, '  gps bias/count, all pressure bins, dtype 1, region 1, used :'
       write(6,900) ( gpswork(k,1,3,1,1) / gpswork(k,1,1,1,1), k=1,np ) 
       print *, ' '
       print *, '  gps rms/count, all pressure bins, dtype 1, region 1, used :'
       write(6,900) ( sqrt( gpswork(k,1,4,1,1) / gpswork(k,1,1,1,1) ), k=1,np )  
       print *, ' '
       print *, '  gps pen, all pressure bins, dtype 1, region 1, used :'
       write(6,900) ( gpswork(k,1,5,1,1), k=1,np ) 
       print *, '  gps qc_pen, all pressure bins, dtype 1, region 1, used :'
       write(6,900) ( gpswork(k,1,6,1,1), k=1,np ) 
       print *, '====================='
       print *, '====================='
       print *, '  gps rejected counts, all pressure bins, dtype 1, region 1, used :'
       write(6,900) (gpswork(k,1,1,1,2),k=1,np) 
       print *, ' '
       print *, '  gps rejected qc counts, all pressure bins, dtype 1, region 1, used :'
       write(6,900) (gpswork(k,1,2,1,1),k=1,np) 
       print *, ' '

!   enddo

   close(81)

   print *, '<-- read_conv'
   return 
end 
