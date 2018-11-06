!   read statistics files from different cycle delay (tm00,tm03 ..)
!   compare their values  


      subroutine check_list_qlyrej(dtype,filetype,n,nd,nd2,nk,ncount1,ncount2,index1,index2,&
                  stdid1,cid1,rmean1,std1,xmed1,break1,std_no1,tdata_no1,iqcflg1,&
                  stdid2,cid2,rmean2,std2,xmed2,break2,std_no2,tdata_no2,iqcflg2,nrecord) 

       character(5) dtype,filetype
       character(1) ddtype
       character(8),dimension(5,n) :: stdid1 
       real(4),dimension(5,n,2) :: cid1
       real(4),dimension(5,n,nk,2) :: tdata_no1
       real(4),dimension(5,n,nk,nd) :: rmean1,std1
       real(4),dimension(5,n,nk) :: std_no1
       real(4),dimension(5,n,nk,nd2) :: xmed1,break1 
       integer,dimension(5,n,nk) :: iqcflg1

       character(8),dimension(n) :: stdid2 
       real(4),dimension(n,2) :: cid2
       real(4),dimension(n,nk,2) :: tdata_no2
       real(4),dimension(n,nk,nd) :: rmean2,std2
       real(4),dimension(n,nk) :: std_no2
       real(4),dimension(n,nk,nd2) :: xmed2,break2
       integer,dimension(n) :: klev
       integer,dimension(n,nk) :: iqcflg2

       print *,'check_list:,ncount1,ncount2=',ncount1,ncount2

!   do i=1,ncount2 
!       write(6,100) stdid1(index2,i),cid1(index2,i,1),cid1(index2,i,2)
!       do k=1,nk
!          if(rmean1(index2,i,k,1) >-900.0) then
!            write(6,110) k, rmean1(index2,i,k,1),rmean1(index2,i,k,2),&
!                        std1(index2,i,k,1),std1(index2,i,k,2)
!            write(6,120) break1(index2,i,k,1),break1(index2,i,k,2),&
!                        break1(index2,i,k,3),break1(index2,i,k,4),&
!                    xmed1(index2,i,k,1),xmed1(index2,i,k,2),&
!                    xmed1(index2,i,k,3),xmed1(index2,i,k,4)
!          endif
!        enddo
!      enddo



    nrecord=ncount2
    do i=1,ncount1
    do j=1,ncount2
       if( trim(stdid1(index1,i)) == trim(stdid1(index2,j))) then
          do k=1,nk
             if(tdata_no1(index1,i,k,1) >= 1.0  .and. &
                tdata_no1(index2,j,k,1) >= 1.0 ) then
                if(tdata_no1(index1,i,k,2) >tdata_no1(index2,j,k,2)) then
                   rmean1(index2,j,k,1:nd) = rmean1(index1,i,k,1:nd)
                   std1(index2,j,k,1:nd) = std1(index1,i,k,1:nd)
                   xmed1(index2,j,k,1:nd2) = xmed1(index1,i,k,1:nd2)
                   break1(index2,j,k,1:nd2) = break1(index1,i,k,1:nd2)
                   std_no1(index2,j,k) = std_no1(index1,i,k)
                   tdata_no1(index2,j,k,1:2)=tdata_no1(index1,i,k,1:2)
                   iqcflg1(index2,j,k)=iqcflg1(index1,i,k)
                endif
             endif
             enddo
             exit
       else if ( j == ncount2) then
          nrecord=nrecord+1
          do k=1,nk
             stdid1(index2,nrecord)=stdid1(index1,i)
             cid1(index2,nrecord,1:2)=cid1(index1,i,1:2)
             rmean1(index2,nrecord,k,1:nd) = rmean1(index1,i,k,1:nd)
             std1(index2,nrecord,k,1:nd) = std1(index1,i,k,1:nd)
             xmed1(index2,nrecord,k,1:nd2) = xmed1(index1,i,k,1:nd2)
             break1(index2,nrecord,k,1:nd2) = break1(index1,i,k,1:nd2)
             std_no1(index2,nrecord,k) = std_no1(index1,i,k)
             tdata_no1(index2,nrecord,k,1:2)=tdata_no1(index1,i,k,1:2)
             iqcflg1(index2,nrecord,k)=iqcflg1(index1,i,k)
          enddo
          print *,'check check_list'
       exit
       endif
    enddo
    enddo


      print *,'check_list:',nrecord

      do i=1, nrecord
       write(6,100) stdid1(index2,i),cid1(index2,i,1),cid1(index2,i,2)
       do k=1,nk
          if(tdata_no1(index2,i,k,1) >-900.0) then
            write(6,110) k, rmean1(index2,i,k,1),rmean1(index2,i,k,2),std1(index2,i,k,1),std1(index2,i,k,2)
            write(6,120) break1(index2,i,k,1),break1(index2,i,k,2),break1(index2,i,k,3),break1(index2,i,k,4),&
                    xmed1(index2,i,k,1),xmed1(index2,i,k,2),xmed1(index2,i,k,3),xmed1(index2,i,k,4)
          endif
        enddo
      enddo

      stdid2(1:nrecord)=stdid1(index2,1:nrecord)
      cid2(1:nrecord,1:2)=cid1(index2,1:nrecord,1:2)
      rmean2(1:nrecord,1:nk,1:nd)=rmean1(index2,1:nrecord,1:nk,1:nd)
      std2(1:nrecord,1:nk,1:nd)=std1(index2,1:nrecord,1:nk,1:nd)
      std_no2(1:nrecord,1:nk)=std_no1(index2,1:nrecord,1:nk)
      xmed2(1:nrecord,1:nk,1:nd2)=xmed1(index2,1:nrecord,1:nk,1:nd2) 
      break2(1:nrecord,1:nk,1:nd2)=break1(index2,1:nrecord,1:nk,1:nd2) 
      tdata_no2(1:nrecord,1:nk,1:2)=tdata_no1(index2,1:nrecord,1:nk,1:2) 
      iqcflg2(1:nrecord,1:nk)=iqcflg1(index2,1:nrecord,1:nk)

100 format(3x,a8,2f8.2)
110 format(i8,4f8.2)
120 format(8f8.2)
                    


        return
        end

