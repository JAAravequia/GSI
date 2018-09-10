!  the program to read prepbufr rawinsonde data



    subroutine read_prepbufr_sfc_q_mor(sub,dtype,rtype,gross)

    real dtpe
!    real(8),dimension(5) :: qms,bac,rcs,ans,pcs
!   real(8),dimension(8) :: vqc
!   real(8),dimension(4) :: obserr 
!   real(8) :: esb
   real(8),dimension(3,15) :: obs
   real(8),dimension(8) :: hdr
   real(8),dimension(7) :: all
   real(8) rstationid,gross

!   character(len=50) :: filein,hdstr,obstr,qmstr,qcstr,rcstr,anstr
!   character(len=50) :: bacstr,vqcstr,esbstr,pcstr,oestr
   character(len=50) :: hdstr,obstr 
    character(len=60) :: allstr 
   character(len=8),dimension(137000) ::  stdid
   character(8) subset,stationid,sub,cdtype
   character*5 dtype
   real rtype
   integer itype,iuse
   
   

    real(4),DIMENSION(137000,2) :: ps180_no
    real(8),DIMENSION(137000,5) :: ps180
    integer,dimension(137000) :: n_ps180
    real(4), dimension(137000,6) :: cid


  data hdstr  /'SID XOB YOB ELV T29 ITP TYP DHR'/
   data allstr/'QQM QAN QFC QOE QVWTG QVWTA ESBAK'/
   data obstr /'QOB QPC QRC'/
   
  data lunin /11/

  equivalence(rstationid,stationid)


  nread=0
  ndata=0
  ps180_no=0.0
  ps180=0.0
  n_ps180=0
  nmiss=0 
  call closbf(lunin)
   open(lunin,file='prepbufr.post',form='unformatted')
   call openbf(lunin,'IN',lunin)
   do while(ireadmg(lunin,subset,idate).eq.0)
      if(SUBSET.NE. sub ) cycle 
      do while(ireadsb(lunin).eq.0)
         call ufbint(lunin,hdr,8,1,iret,hdstr)   ! read header from one observation
         call ufbint(lunin,all,7,1,iret,allstr)   !  read all the data
         call ufbevn(lunin,obs,3,1,15,iret,obstr)  !  read different 
       if(hdr(2) <0.0) hdr(2)=360.00+hdr(2)
       if(hdr(2) >360.00) then
         print *,'read_prepbufr_sfc_q_mor:problem with longitudedtype=',dtype,hdr(1),hdr(2),hdr(3)
         cycle
       endif
       if(abs(hdr(3)) >90.0) then
         print *,'read_prepbufr_sfc_q_mor:problem with latitude, dtype=',dtype,hdr(1),hdr(2),hdr(3)
         cycle
       endif
       if(hdr(7) /= rtype .or. all(3) >80000.0 ) cycle
       if(hdr(7) == rtype .and. all(3) <80000.0) then
          nread=nread+1
          if(nread==1) then
             ndata=ndata+1
             rstationid=hdr(1)
             stdid(ndata)=stationid 
             cid(ndata,1:6)=hdr(2:7)
          else
             do i=ndata,1,-1
                rstationid=hdr(1)
                if(stationid == stdid(i))then
                   exit
                else
                   if( i == 1) then
                      ndata=ndata+1
                      rstationid=hdr(1)
                      stdid(ndata)=stationid
                      cid(ndata,1:6)=hdr(2:7)
                   endif
                endif
             enddo
          endif
          if (nread <10) then
             print *,nread,ndata,stationid,stdid(ndata),obs(1,1),obs(1,2),obs(1,3),all(1),all(2),all(3)
             print *,obs(2,1),obs(2,2),obs(2,3),obs(3,1),obs(3,2),obs(3,3),all(4),all(5),all(6),all(7)
          endif
          n_ps180(ndata)=n_ps180(ndata)+1
          nn=n_ps180(ndata)
          j=0
          do i=1,4
             if(obs(1,i) <80000.00 .and. obs(2,i) /=17.0) then
                j=i
                exit
             endif
          enddo
          if( j==0) then
             print *,'read_prepbufr_sfc_q:no q observation read,ndata=',ndata
          else if( all(4) <1000.0) then
             if(all(1)  <0.0 .or. all(1) ==10.0) then                 ! qm=10, rejected by GSI
                 ps180_no(ndata,2)=ps180_no(ndata,2)+1.0
             else if(obs(1,1) <80000.0 .and. all(3) <80000.0) then
                if(all(4) >100.0) all(4) =2.0
                ratio=10.0*abs(obs(1,j)-all(3))/(all(7)*all(4))
                if(ratio >gross) then
                   ps180_no(ndata,2)=ps180_no(ndata,2)+1.0
                else
                   ps180_no(ndata,1)=ps180_no(ndata,1)+1.0
                   ps180(ndata,1)=ps180(ndata,1)+10.0*(obs(1,j)-all(3))/all(7)  ! o-b
                   ps180(ndata,2)=ps180(ndata,2)+10.0*(obs(1,j)-all(2))/all(7)  ! o-a
                   ps180(ndata,3)=ps180(ndata,3)+all(5)  !var. qc weight
                   ps180(ndata,4)=ps180(ndata,4)+all(6)  !var. qc weight
                   ps180(ndata,5)=ps180(ndata,5)+10.0*obs(1,j)/all(7)  !observations
                endif  
             endif  
          else if(all(4) >1000.0) then
             if(all(1)  <0.0  .or. all(1) ==10.0) then
                ps180_no(ndata,2)=ps180_no(ndata,2)+1.0
             else if(obs(1,1) <80000.0 .and. all(3) <80000.0) then
                ratio=10.0*abs(obs(1,j)-all(3))/(all(7)*2.0)
!                     print *,'ndata,stationid,all(1),ratio,gross=',ndata,stationid,all(1),ratio,gross,obs(1,j),all(3)
!                        print *,'ndata,stationid,all(1),ratio,gross=',ndata,stationid,all(1),ratio,gross,j,obs(1,1),all(3)
                if(ratio >gross) then
                   ps180_no(ndata,2)=ps180_no(ndata,2)+1.0
                else
                   ps180_no(ndata,1)=ps180_no(ndata,1)+1.0
                   ps180(ndata,1)=ps180(ndata,1)+10.0*(obs(1,j)-all(3))/all(7)  ! o-b
                   ps180(ndata,2)=ps180(ndata,2)+10.0*(obs(1,j)-all(2))/all(7)  ! o-a
                   ps180(ndata,3)=ps180(ndata,3)+all(5)  !var. qc weight
                   ps180(ndata,4)=ps180(ndata,4)+all(6)  !var. qc weight
                   ps180(ndata,5)=ps180(ndata,5)+10.0*obs(1,j)/all(7)  !observations
                endif
             endif          ! end of quality 
          endif                !  end of j 
       endif                !  end of  rtype 

     enddo      !! enddo ireadsub
   enddo      !! enddo  ireadmg 

           print *,'read_prepbufr_sfc_ps ',dtype,nread,ndata,nmiss

           do i=1,ndata
             do j=1,5
               if(ps180_no(i,1) >=1.0) then
                 ps180(i,j)=ps180(i,j)/ps180_no(i,1)
               else
                 ps180(i,j)=-999.0
               endif
             enddo
           enddo

          dtype=trim(dtype) 

          write(6,1000) (ps180_no(i,1),i=1,30)
          write(6,1000) (ps180_no(i,2),i=1,30)
          write(6,1000) (ps180(i,1),i=1,30)
1000 format(10f8.1)


        if(ndata >0) then
         open(10,file=dtype,form='unformatted')
         write(10) ndata
         write(10) stdid(1:ndata),cid(1:ndata,1:6),ps180_no(1:ndata,1:2),ps180(1:ndata,1:5)
         close(10)
        endif

           

          close(11) 
          return
          end     
