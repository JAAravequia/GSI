!  the program set criteria for potential rejection list of stations and bias list
!  of the surface pressure stations



        subroutine w_selet_sfc_reg(dtype,iqcflg)

!!!    get criteria from crite_constnt module
  
      use crite_constnt_reg,only: wd_c_rm,wd_c_std,ws_c_rm,ws_c_std

     real(4),allocatable,dimension(:) :: tmp2
!      character(len=8),dimension(35000) ::  stdid
!     real(4), dimension(35000,6) :: cid
!     real(4),dimension(35000,6) :: std,std_no,rmean,break,xstas
!     real(4),dimension(35000,2) :: xmed,smad
!     real(4),dimension(35000,3) :: tdata_no2
!     real(4),dimension(35000,400) :: tdata_no3
!     integer, dimension(35000) :: n_tdata,n2_tdata

      character(len=8),allocatable,dimension(:) :: stdid
      real(4),allocatable,dimension(:,:) :: cid,std,std_no,rmean,break,xstas
      real(4),allocatable,dimension(:,:) :: xmed,smad,tdata_no2,tdata_no3
      integer,allocatable,dimension(:) :: n_tdata,n2_tdata
     
     character(5) dtype
     character(8)  stid
     character(50) outfile,outfile1,outfile2,outfile3,outfile4,outfile5,outfile6 
     character(50) file1,file2
     integer,dimension(4) :: iqcflg

     real(4) rtim,nflg0,xlat0,xlon0,nlev0,rlat,rlon
    


          
  outfile=trim(dtype)//'_qlyrej_list'
  outfile1=trim(dtype)//'_susplist'
  outfile2=trim(dtype)//'_rej_dir_list'
  outfile3=trim(dtype)//'_rej_sp_list'
  outfile4=trim(dtype)//'_other_list'
  outfile5=trim(dtype)//'_susplist_stas_station'
  outfile6=trim(dtype)//'_norm_list'

   open(70,file=outfile,form='formatted')
   open(71,file=outfile1,form='formatted')
   open(72,file=outfile2,form='formatted')
   open(73,file=outfile3,form='formatted')
   open(74,file=outfile4,form='formatted')
   open(75,file=outfile5,form='unformatted')
   open(76,file=outfile6,form='formatted')

!  open input file and file names

    file1=trim(dtype)//'_station'

     open(11,file=file1,form='unformatted')
     read(11) ndata,nread,n_miss_crit

     allocate(stdid(ndata),cid(ndata,6),tdata_no2(ndata,3))
     allocate(tdata_no3(ndata,nread),n_tdata(ndata),n2_tdata(ndata))
     allocate(std(ndata,6),std_no(ndata,6),rmean(ndata,6),break(ndata,6))
     allocate(xmed(ndata,2),smad(ndata,2))
     allocate(xstas(ndata,6))

     read(11) stdid(1:ndata),cid(1:ndata,:)
     read(11) n_tdata(1:ndata)
     read(11) tdata_no2(1:ndata,:)
     read(11) tdata_no3(1:ndata,1:nread)

      write(6,*) 'ndata=',ndata
      write(6,500) (stdid(i),i=1,10)
      write(6,510) (cid(i,1),i=1,10)
      write(6,520) (n_tdata(i),i=1,10)

500 format(10a8)
510 format(10f8.2)
520 format(10i8)

!  open statistics file

     file2=trim(dtype)//'_stas'

      open(21,file=file2,form='unformatted')

       read(21) rmean(1:ndata,:)
       read(21) std(1:ndata,:)
       read(21) xstas(1:ndata,:)
       read(21) break(1:ndata,:)
       read(21) std_no(1:ndata,:)

          xmed(1:ndata,1:2)=xstas(1:ndata,1:2)
          smad(1:ndata,1:2)=xstas(1:ndata,3:4)


          close(11)
          close(21)

         write(6,600) (rmean(i,1),i=1,10)
      write(6,600) (std(i,1),i=1,10)
      write(6,600) (break(i,1),i=1,10)
600 format(10f8.2)

!! creat normal station frequecy criteria

    n_norm_crit=0
    if(trim(dtype) == 'ps120') then
       n_norm_crit=nread/10
    else
       n_norm_crit=nread/5
    endif

    n2_tdata=0
    do i=1,ndata
    do j=1,nread
      if(tdata_no3(i,j) >0.0) then
        n2_tdata(i)=n2_tdata(i)+1
      endif
    enddo
    enddo

   

  n_rej_dir=0
  n_rej_sp=0
  n_qly=0
  n_sup=0
  n_other=0
  n_norm=0
  nflag=1
  rtim=0.0
  nflg0=0
  xlat0=0.0
  xlon0=0.0
  nlev0=0
  stid='        '
  
  rm_c=0.8*wd_c_rm           !! the standard to reject the station which has data 70%
  std_c=0.8*wd_c_std        !!! rejected by quality control

  allocate(tmp2(12))
    do i=1,ndata
       if(cid(i,6) <-50.0 ) then
          n_rej_dir=n_rej_dir+1
          write(72,100) stdid(i),i,cid(i,1),cid(i,2),std_no(i,1),rmean(i,1),rmean(i,3),&
                        std(i,1),std(i,3),tdata_no2(i,1),tdata_no2(i,2),iqcflg(2)
          write(72,110) xmed(i,1),xmed(i,2),smad(i,1),smad(i,2),rmean(i,2),rmean(i,4),std(i,2),std(i,4)
          write(72,120) n_tdata(i),break(i,1),break(i,2),break(i,3),break(i,4)
       else if(rmean(i,1) >-900.00 .and. std(i,1) >-900.0) then
          ag=abs(rmean(i,1))
          if(abs(cid(i,2)) <65.0 ) then
             if( tdata_no2(i,3) >=0.80 .and. (abs(rmean(i,1)) >= rm_c .or. std(i,1) >= std_c) ) then
                n_qly=n_qly+1
                write(70,100) stdid(i),i,cid(i,1),cid(i,2),std_no(i,1),rmean(i,1),rmean(i,3),&
                       std(i,1),std(i,3),tdata_no2(i,1),tdata_no2(i,2),iqcflg(1)
                write(70,110) xmed(i,1),xmed(i,2),smad(i,1),smad(i,2),rmean(i,2),rmean(i,4),std(i,2),std(i,4)
                write(70,120) n_tdata(i),break(i,1),break(i,2),break(i,3),break(i,4)
             else if(std_no(i,1) >=20.0 .and. n_tdata(i) < n_miss_crit) then
                if(ag >=wd_c_rm .or.  std(i,1) >=wd_c_std  .or. &
                   abs(rmean(i,3)) >=ws_c_rm  .or. std(i,3) >=ws_c_std ) then
                   stid=stdid(i)
                   rlat=cid(i,2)
                   rlon=cid(i,1)
                   write(75) stid,rlat,rlon,rtim,1,1
                   tmp2(1)=rmean(i,1)
                   tmp2(2)=rmean(i,3)
                   tmp2(3)=rmean(i,5)
                   tmp2(4)=std(i,1)
                   tmp2(5)=std(i,3)
                   tmp2(6)=std(i,5)    !obs
                   tmp2(7)=xmed(i,1)
                   tmp2(8)=xmed(i,2)
                   tmp2(9)=smad(i,1)
                   tmp2(10)=smad(i,2)
                   tmp2(11)=std_no(i,1)
                   tmp2(12)=tdata_no2(i,2)
                   write(75) tmp2
                   n_sup=n_sup+1
                   write(71,100) stdid(i),i,cid(i,1),cid(i,2),std_no(i,1),rmean(i,1),rmean(i,3),&
                          std(i,1),std(i,3),tdata_no2(i,1),tdata_no2(i,2),iqcflg(4)
                   write(71,110) xmed(i,1),xmed(i,2),smad(i,1),smad(i,2),rmean(i,2),rmean(i,4),std(i,2),std(i,4)
                   write(71,120) n_tdata(i),break(i,1), break(i,2),break(i,3),break(i,4)
                   if( break(i,4) >0.0) then
                      if(std(i,1) >=wd_c_std .or. abs(rmean(i,1)) >=45.0) then
                         n_rej_dir=n_rej_dir+1
                         write(72,100) stdid(i),i,cid(i,1),cid(i,2),std_no(i,1),rmean(i,1),rmean(i,3),&
                              std(i,1),std(i,3),tdata_no2(i,1),tdata_no2(i,2),iqcflg(3)
                         write(72,110) xmed(i,1),xmed(i,2),smad(i,1),smad(i,2),rmean(i,2),rmean(i,4),std(i,2),std(i,4)
                         write(72,120) n_tdata(i),break(i,1),break(i,2),break(i,3),break(i,4)
                      else if(std(i,3) >ws_c_std) then
                         n_rej_sp=n_rej_sp+1
                         write(73,100) stdid(i),i,cid(i,1),cid(i,2),std_no(i,1),rmean(i,1),rmean(i,3),&
                              std(i,1),std(i,3),tdata_no2(i,1),tdata_no2(i,2),iqcflg(3)
                         write(73,110) xmed(i,1),xmed(i,2),smad(i,1),smad(i,2),rmean(i,2),rmean(i,4),std(i,2),std(i,4)
                         write(73,120) n_tdata(i),break(i,1),break(i,2),break(i,3),break(i,4)
                      else
                         n_norm=n_norm+1
                         write(76,100) stdid(i),i,cid(i,1),cid(i,2),std_no(i,1),rmean(i,1),rmean(i,3),&
                              std(i,1),std(i,3),tdata_no2(i,1),tdata_no2(i,2),iqcflg(4)
                         write(76,110) xmed(i,1),xmed(i,2),smad(i,1),smad(i,2),rmean(i,2),rmean(i,4),std(i,2),std(i,4)
                         write(76,120) n_tdata(i),break(i,1), break(i,2),break(i,3),break(i,4)
                      endif
                   else
                      n_norm=n_norm+1
                      write(76,100) stdid(i),i,cid(i,1),cid(i,2),std_no(i,1),rmean(i,1),rmean(i,3),&
                              std(i,1),std(i,3),tdata_no2(i,1),tdata_no2(i,2),iqcflg(4)
                      write(76,110) xmed(i,1),xmed(i,2),smad(i,1),smad(i,2),rmean(i,2),rmean(i,4),std(i,2),std(i,4)
                      write(76,120) n_tdata(i),break(i,1), break(i,2),break(i,3),break(i,4)
                   endif
                else
                   n_norm=n_norm+1
                   write(76,100) stdid(i),i,cid(i,1),cid(i,2),std_no(i,1),rmean(i,1),rmean(i,3),&
                              std(i,1),std(i,3),tdata_no2(i,1),tdata_no2(i,2),iqcflg(4)
                   write(76,110) xmed(i,1),xmed(i,2),smad(i,1),smad(i,2),rmean(i,2),rmean(i,4),std(i,2),std(i,4)
                   write(76,120) n_tdata(i),break(i,1), break(i,2),break(i,3),break(i,4)
                endif
             else
                n_norm=n_norm+1
                write(76,100) stdid(i),i,cid(i,1),cid(i,2),std_no(i,1),rmean(i,1),rmean(i,3),&
                              std(i,1),std(i,3),tdata_no2(i,1),tdata_no2(i,2),iqcflg(4)
                write(76,110) xmed(i,1),xmed(i,2),smad(i,1),smad(i,2),rmean(i,2),rmean(i,4),std(i,2),std(i,4)
                write(76,120) n_tdata(i),break(i,1), break(i,2),break(i,3),break(i,4)
             endif
          else
             n_norm=n_norm+1
             write(76,100) stdid(i),i,cid(i,1),cid(i,2),std_no(i,1),rmean(i,1),rmean(i,3),&
                          std(i,1),std(i,3),tdata_no2(i,1),tdata_no2(i,2),iqcflg(4)
             write(76,110) xmed(i,1),xmed(i,2),smad(i,1),smad(i,2),rmean(i,2),rmean(i,4),std(i,2),std(i,4)
             write(76,120) n_tdata(i),break(i,1), break(i,2),break(i,3), break(i,4)
          endif 
       endif 
    enddo

100 format(3x,a8,i8,9f8.2,i3)
110 format(3x,8f8.2)
120 format(3x,i8,4f8.2)

deallocate(tmp2)

       Print *,'w_selet_sfc_reg:,dtype,ndata,n_norm,n_qly,n_sup,n_rej_dir,n_rej_sp,n_other'
       Print *,'w_selet_sfc_reg:',dtype,ndata,n_norm,n_qly,n_sup,n_rej_dir,n_rej_sp,n_other

!!  the end of file grads file
     stid='        '
     write(75) stid,xlat0,xlon0,rtim,nlev0,nflag0
    
     close(70)
     close(71)
     close(72)
     close(73)
     close(74)
     close(75)
     close(76)
     close(21)

     deallocate(stdid,cid,tdata_no2,tdata_no3,n_tdata,n2_tdata,std,std_no,&
               rmean,break,xmed,smad,xstas)

     return
     end

