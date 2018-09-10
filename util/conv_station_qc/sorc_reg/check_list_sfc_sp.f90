!   read statistics files from different cycle delay (tm00,tm03 ..)
!   compare their values  


      subroutine check_list_sfc_sp(dtype,filetype,n,nd,nd2,ncount1,ncount2,index1,index2,&
                  stdid1,cid1,rmean1,std1,xmed1,break1,std_no1,tdata_no1,iqcflg1,&
                  stdid2,cid2,rmean2,std2,xmed2,break2,std_no2,tdata_no2,iqcflg2,nrecord) 

       character(5) dtype,filetype
       character(1) ddtype
       character(8),dimension(5,n) :: stdid1 
       real(4),dimension(5,n,2) :: cid1,tdata_no1
       real(4),dimension(5,n,nd) :: rmean1,std1
       real(4),dimension(5,n) :: std_no1
       real(4),dimension(5,n,nd2) :: xmed1,break1 
       integer,dimension(5,n) :: iqcflg1

       character(8),dimension(n) :: stdid2 
       real(4),dimension(n,2) :: cid2,tdata_no2
       real(4),dimension(n,nd) :: rmean2,std2
       real(4),dimension(n) :: std_no2
       real(4),dimension(n,nd2) :: xmed2,break2
       integer,dimension(n) :: iqcflg2

    nrecord=ncount2
    do i=1,ncount1
    do j=1,ncount2
       if( trim(stdid1(index1,i)) == trim(stdid1(index2,j))) then
          if(trim(filetype) == 'bias') then
             if(abs(rmean1(index1,i,3)) <abs(rmean1(index2,j,3))) then
                rmean1(index2,j,1:nd) = rmean1(index1,i,1:nd)
                std1(index2,j,1:nd) = std1(index1,i,1:nd)
                xmed1(index2,j,1:nd2) = xmed1(index1,i,1:nd2)
                break1(index2,j,1:nd2) = break1(index1,i,1:nd2)
                std_no1(index2,j) = std_no1(index1,i)
                tdata_no1(index2,j,1:2)=tdata_no1(index1,i,1:2)
                iqcflg1(index2,j)=iqcflg1(index1,i)
                exit
             endif
            exit
          else if(trim(filetype) == 'rej') then
             if(std1(index1,i,3) <std1(index2,j,3)) then
                rmean1(index2,j,1:nd) = rmean1(index1,i,1:nd)
                std1(index2,j,1:nd) = std1(index1,i,1:nd)
                xmed1(index2,j,1:nd2) = xmed1(index1,i,1:nd2)
                break1(index2,j,1:nd2) = break1(index1,i,1:nd2)
                std_no1(index2,j) = std_no1(index1,i)
                tdata_no1(index2,j,1:2)=tdata_no1(index1,i,1:2)
                iqcflg1(index2,j)=iqcflg1(index1,i)
                exit
             endif
             exit
          endif
           exit
       else if ( j == ncount2) then
          nrecord=nrecord+1
          stdid1(index2,nrecord)=stdid1(index1,i)
          cid1(index2,nrecord,1:2)=cid1(index1,i,1:2)
          rmean1(index2,nrecord,1:nd) = rmean1(index1,i,1:nd)
          std1(index2,nrecord,1:nd) = std1(index1,i,1:nd)
          xmed1(index2,nrecord,1:nd2) = xmed1(index1,i,1:nd2)
          break1(index2,nrecord,1:nd2) = break1(index1,i,1:nd2)
          std_no1(index2,nrecord) = std_no1(index1,i)
          tdata_no1(index2,nrecord,1:2)=tdata_no1(index1,i,1:2)
          iqcflg1(index2,nrecord)=iqcflg1(index1,i)
          print *,'check list_sfc'
          print *,nrecord, stdid1(index2,nrecord),cid1(index2,nrecord,1),rmean1(index2,nrecord,1),&
                  std1(index2,nrecord,1),xmed1(index2,nrecord,1),break1(index2,nrecord,1),std_no1(index2,nrecord),tdata_no1(index2,nrecord,1)
       exit
       endif
    enddo
    enddo


      print *,'check_list_sfc_sp:',nrecord

!      do i=1, nrecord
!       write(6,100) i,stdid1(index2,i),cid1(index2,i,1),cid1(index2,i,2),rmean1(index2,i,1),rmean1(index2,i,2),std1(index2,i,1),std1(index2,i,2)
!       write(6,110) break1(index2,i,1),break1(index2,i,2),break1(index2,i,3),break1(index2,i,4),&
!                    xmed1(index2,i,1),xmed1(index2,i,2),xmed1(index2,i,3),xmed1(index2,i,4)
!      enddo

      stdid2(1:nrecord)=stdid1(index2,1:nrecord)
      cid2(1:nrecord,1:2)=cid1(index2,1:nrecord,1:2)
      rmean2(1:nrecord,1:nd)=rmean1(index2,1:nrecord,1:nd)
      std2(1:nrecord,1:nd)=std1(index2,1:nrecord,1:nd)
      std_no2(1:nrecord)=std_no1(index2,1:nrecord)
      xmed2(1:nrecord,1:nd2)=xmed1(index2,1:nrecord,1:nd2) 
      break2(1:nrecord,1:nd2)=break1(index2,1:nrecord,1:nd2) 
      tdata_no2(1:nrecord,1:2)=tdata_no1(index2,1:nrecord,1:2) 
      iqcflg2(1:nrecord)=iqcflg1(index2,1:nrecord)

100 format(i5,3x,a8,6f8.2)
110 format(8f8.2)
                    


        return
        end

