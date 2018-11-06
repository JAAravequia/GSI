!  the program set criteria for potential rejection list of stations and bias list
!  of the surface pressure stations



        subroutine w_selet(dtype,nk,iqcflg)

!!!    get criteria from crite_constnt module
  
      use crite_constnt,only: wd2_c_rm,wd2_c_std,ws2_c_rm,ws2_c_std

     character(len=8),allocatable,dimension(:) :: stdid
     real(4),allocatable,dimension(:,:,:) :: std,std_no,rmean,break,xmed,smad
    real(4),allocatable,dimension(:,:) :: cid
    real(4),allocatable,dimension(:,:,:) :: tdata_no3,tdata_no2,xstas
    integer,allocatable,dimension(:,:) :: n_tdata,n2_tdata
    integer,allocatable,dimension(:,:) :: index,index_rejsp,index_rejdir,index_other,index_qly
    real(4),allocatable,dimension(:) :: index2,index2_rejdir,index2_rejsp,index2_other,index2_qly,index2_norm

     real(4),allocatable,dimension(:) :: tmp2
!      character(len=8),dimension(2000) ::  stdid
!     real(4), dimension(2000,6) :: cid
!     real(4),dimension(2000,nk,6) :: std,std_no,rmean,break,xstas
!     real(4),dimension(2000,nk,2) :: xmed,smad
!     real(4),dimension(2000,nk,3) :: tdata_no2
!     real(4),dimension(2000,nk,400) :: tdata_no3
!     integer, dimension(2000,nk) :: n_tdata,index,index_rejdir,index_rejsp,index_other,index_qly
!     integer, dimension(2000,nk) :: n2_tdata
!     real(4),dimension(2000) :: index2,index2_rejdir,index2_rejsp,index2_other,index2_qly,index2_norm
     real(4),dimension(43) :: plevel
     integer, dimension(nk) :: n_rej_dir,n_rej_sp,n_sup,n_other,n_qly
     real(4),dimension(nk) :: rm_c,std_c,w_c_std2 
     real(4),dimension(10) :: plev_man
     integer,dimension(4) :: iqcflg
     
     character(5) dtype
     character(8)  stid
     character(50) outfile,outfile1,outfile2,outfile3,outfile4,outfile5,outfile6 
     character(50) file1,file2

     real(4) rtim,nflg0,xlat0,xlon0,nlev0,rlat,rlon

     data plevel/1000.,975.,950.,925.,900.,875.,850.,825.,800.,775.,750.,725.,700.,675.,650.,625.,600.,575.,550.,525.,500.,475.,450.,425.,400.,375.,350.,325.,300.,275.,250.,225.,200.,175.,150.,125.,100.,70.,50.,30.,20.,10.,5./


     data plev_man /1000.0,925.0,850.,700.,500.,300.,250.,200.,150.,100/
    
   print *,'w_select:',nk,nob,ndata


          
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
     rewind(11)
     read(11) ndata,nread,n_miss_crit

     print *,'ndata,nread,n_miss_crit=',ndata,nread,n_miss_crit

   allocate(stdid(ndata),cid(ndata,6))
   allocate(tdata_no2(ndata,nk,3),n_tdata(ndata,nk),tdata_no3(ndata,nk,nread))
   allocate(std(ndata,nk,6),std_no(ndata,nk,6),rmean(ndata,nk,6))
   allocate(xstas(ndata,nk,6),break(ndata,nk,6))
   allocate(xmed(ndata,nk,2),smad(ndata,nk,2))
   allocate(index(ndata,nk),index_rejsp(ndata,nk),index_rejdir(ndata,nk))
   allocate(index_other(ndata,nk),index_qly(ndata,nk),n2_tdata(ndata,nk))
   
   allocate(index2(ndata),index2_rejsp(ndata),index2_rejdir(ndata))
   allocate(index2_other(ndata),index2_qly(ndata),index2_norm(ndata))


     read(11) stdid(1:ndata)
     read(11) cid(1:ndata,:) 
     read(11) n_tdata(1:ndata,1:nk) 
     read(11) tdata_no2(1:ndata,1:nk,1:3) 
     read(11) tdata_no3(1:ndata,1:nk,1:nread) 


      write(6,*) 'ndata=',ndata
      write(6,500) (stdid(i),i=1,10)
      write(6,510) (cid(i,1),i=1,10)
      write(6,520) (n_tdata(i,3),i=1,10)

500 format(10a8)
510 format(10f8.2)
520 format(10i8)

!  open statistics file

     file2=trim(dtype)//'_stas'

      open(21,file=file2,form='unformatted')

      do k=1,nk
       read(21) rmean(1:ndata,k,:)
     enddo
     do k=1,nk
         read(21) std(1:ndata,k,:)
      enddo
     do k=1,nk
         read(21) xstas(1:ndata,k,:)
      enddo

     do k=1,nk
         read(21) break(1:ndata,k,:)
      enddo
     do k=1,nk
         read(21) std_no(1:ndata,k,:)
      enddo

       xmed(1:ndata,1:nk,1:2)=xstas(1:ndata,1:nk,1:2)
       smad(1:ndata,1:nk,1:2)=xstas(1:ndata,1:nk,3:4)


       close(11)
       close(21)

       write(6,600) (rmean(i,5,1),i=1,10)
      write(6,600) (std(i,6,1),i=1,10)
      write(6,600) (break(i,7,1),i=1,10)
600 format(10f8.2)

!! creat normal station frequecy criteria

    n=0
    if(trim(dtype) == 'uv220' .or. trim(dtype) == 'uv229') then
       n_norm_crit=nread/10
    else
       n_norm_crit=nread/5
    endif

     print *,'n_norm_crit=',n_norm_crit

    n2_tdata=0
    do i=1,ndata
    do j=1,nread
    do k=1,nk
      if(tdata_no3(i,k,j) >0.0) then
        n2_tdata(i,k)=n2_tdata(i,k)+1
      endif
    enddo
    enddo
    enddo


  n_rej_dir=0
  n_rej_sp=0
  n_sup=0
  n_other=0
  n_qly=0
  nflag=1
  rtim=0.0
  nflg0=0
  xlat0=0.0
  xlon0=0.0
  nlev0=0
  stid='        '
  index=-1
  index_rejsp=-1
  index_rejdir=-1
  index_other=-1
  index_qly=-1
  index2=0.0
  index2_rejdir=0.0
  index2_rejsp=0.0
  index2_other=0.0
  index2_qly=0.0
  index2_norm=0.0
  n_rej=0
  n_bias=0
  n_other=0
  n_qly=0
  n_sup=0


  
  rm_c=0.8*wd2_c_rm           !! the standard to reject the station which has data 70%
  std_c=0.8*wd2_c_std        !!! rejected by quality control

  allocate(tmp2(12))
    do i=1,ndata
    do k=1,nk
       ag=abs(rmean(i,k,1))
       if( tdata_no2(i,k,3) >=0.80 .and. (abs(rmean(i,k,1)) >= rm_c(k).or. std(i,k,1) >= std_c(k)) ) then
!          print *,'w-selet ',tdata_no2(i,k,3),tdata_no2(i,k,1),tdata_no2(i,k,2)          
           n_qly(k)=n_qly(k)+1
           index_qly(i,k)=1
           index2_qly(i)= index2_qly(i)+1.0
       else if(k <37 .and. abs(cid(i,2)) <65.0 .and. std_no(i,k,1) >=20.0 .and. n_tdata(i,k) <n_miss_crit) then
           kkk=0
           do kk=1,10
             if(plevel(k) == plev_man(kk)) then
              kkk=kk
              exit
             endif
           enddo
           if ( kkk /=0) then
              if(n2_tdata(i,k) >n_norm_crit .and. ag <wd2_c_rm(k) .and. std(i,k,1) <wd2_c_std(k) .and. &
                 abs(rmean(i,k,3)) <ws2_c_rm(k)  .or. std(i,k,3) <ws2_c_std(k) ) then
                 index2_norm(i)=index2_norm(i)+1.0
              endif
           endif
           if(ag >=wd2_c_rm(k) .or.  std(i,k,1) >=wd2_c_std(k)  .or. &
              abs(rmean(i,k,3)) >=ws2_c_rm(k)  .or. std(i,k,3) >=ws2_c_std(k) ) then
              stid=stdid(i)
              rlat=cid(i,2)
              rlon=cid(i,1)
              write(75) stid,rlat,rlon,rtim,1,0
              tmp2(1)=rmean(i,k,1)
              tmp2(2)=rmean(i,k,3)
              tmp2(3)=rmean(i,k,5)
              tmp2(4)=std(i,k,1)
              tmp2(5)=std(i,k,3)
              tmp2(6)=std(i,k,5)    !obs
              tmp2(7)=xmed(i,k,1)
              tmp2(8)=xmed(i,k,2)
              tmp2(9)=smad(i,k,1)
              tmp2(10)=smad(i,k,2)
              tmp2(11)=std_no(i,k,1)
              tmp2(12)=tdata_no2(i,k,2)
              write(75) plevel(k),tmp2
              n_sup(k)=n_sup(k)+1
              index(i,k)=1
              index2(i)= index2(i)+1.0
              if(break(i,k,4) >0.0) then
                 if(std(i,k,1) >=wd2_c_std(k).or. abs(rmean(i,k,1)) >=45.0) then
                    if(abs(rlat) >11.0 ) then
                    n_rej_dir(k)=n_rej_dir(k)+1
                    index_rejdir(i,k)=1
                    index2_rejdir(i)= index2_rejdir(i)+1.0
!               else if(abs(rlat) <=11.0 .and. k<=32) then
!                  n_rej_dir(k)=n_rej_dir(k)+1
!                  index_rejdir(i,k)=1
!                  index2_rejdir(i)= index2_rejdir(i)+1.0
               endif
              else if(std(i,k,3) >ws2_c_std(k)) then
                 if(abs(rlat) >11.0 ) then
                    index_rejsp(i,k)=1
                    index2_rejsp(i)= index2_rejsp(i)+1.0
!                else if(abs(rlat) <=11.0 .and. k<=32) then
!                    index_rejsp(i,k)=1
!                    index2_rejsp(i)= index2_rejsp(i)+1.0
                 endif
              else 
                 if(abs(rlat) >11.0 ) then
                    n_other(k)=n_other(k)+1
                    index_other(i,k)=1
                    index2_other(i)= index2_other(i)+1.0
!                else if(abs(rlat) <=11.0 .and. k<=32) then
!                   n_other(k)=n_other(k)+1
!                   index_other(i,k)=1
!                 index2_other(i)= index2_other(i)+1.0
                endif
              endif
            endif
            endif
     endif 
  enddo
 enddo

    do i=1,ndata


     if(index2_qly(i) >=4.0) then
      write(70,105) stdid(i),i,cid(i,1),cid(i,2),index2_qly(i) 
      do k=1,nk
       if(index_qly(i,k) >0) then
         write(70,100) std_no(i,k,1),rmean(i,k,1),rmean(i,k,3),& 
                      std(i,k,1),std(i,k,3),tdata_no2(i,k,1),tdata_no2(i,k,2),iqcflg(1) 
         write(70,110) k,xmed(i,k,1),xmed(i,k,2),smad(i,k,1),smad(i,k,2)
         write(70,120) break(i,k,1), break(i,k,2),break(i,k,3),break(i,k,4),&
                       rmean(i,k,2),rmean(i,k,4),std(i,k,2),std(i,k,4) 
       endif
      enddo
     endif

     if(index2(i) >=4.0) then
      write(71,105) stdid(i),i,cid(i,1),cid(i,2),index2(i) 
      do k=1,nk
       if(index(i,k) >0) then
         write(71,100) std_no(i,k,1),rmean(i,k,1),rmean(i,k,3),& 
                      std(i,k,1),std(i,k,3),tdata_no2(i,k,1),tdata_no2(i,k,2),iqcflg(4) 
         write(71,110) k,xmed(i,k,1),xmed(i,k,2),smad(i,k,1),smad(i,k,2)
         write(71,120) break(i,k,1), break(i,k,2),break(i,k,3),break(i,k,4),&
                       rmean(i,k,2),rmean(i,k,4),std(i,k,2),std(i,k,4) 
       endif
      enddo
     endif

     if(index2_rejdir(i) >=4.0) then
      write(72,105) stdid(i),i,cid(i,1),cid(i,2),index2_rejdir(i)
      do k=1,nk
       if(index_rejdir(i,k) >0) then
         write(72,100) std_no(i,k,1),rmean(i,k,1),rmean(i,k,3),& 
                      std(i,k,1),std(i,k,3),tdata_no2(i,k,1),tdata_no2(i,k,2),iqcflg(3) 
         write(72,110) k,xmed(i,k,1),xmed(i,k,2),smad(i,k,1),smad(i,k,2)
         write(72,120) break(i,k,1), break(i,k,2),break(i,k,3),break(i,k,4),&
                       rmean(i,k,2),rmean(i,k,4),std(i,k,2),std(i,k,4) 
       endif
      enddo
     endif

     if(index2_rejsp(i) >=4.0) then
      write(73,105) stdid(i),i,cid(i,1),cid(i,2),index2_rejsp(i)
      do k=1,nk
       if(index_rejsp(i,k) >0) then
         write(73,100) std_no(i,k,1),rmean(i,k,1),rmean(i,k,3),& 
                      std(i,k,1),std(i,k,3),tdata_no2(i,k,1),tdata_no2(i,k,2),iqcflg(3) 
         write(73,110) k,xmed(i,k,1),xmed(i,k,2),smad(i,k,1),smad(i,k,2)
         write(73,120) break(i,k,1), break(i,k,2),break(i,k,3),break(i,k,4),&
                       rmean(i,k,2),rmean(i,k,4),std(i,k,2),std(i,k,4) 
       endif
      enddo
     endif

     if(index2_other(i) >=4.0) then
      write(74,105) stdid(i),i,cid(i,1),cid(i,2),index2_other(i)
      do k=1,nk
       if(index_other(i,k) >0) then
         write(74,100) std_no(i,k,1),rmean(i,k,1),rmean(i,k,3),& 
                      std(i,k,1),std(i,k,3),tdata_no2(i,k,1),tdata_no2(i,k,2),iqcflg(4) 
         write(74,110) k,xmed(i,k,1),xmed(i,k,2),smad(i,k,1),smad(i,k,2)
         write(74,120) break(i,k,1), break(i,k,2),break(i,k,3),break(i,k,4),&
                       rmean(i,k,2),rmean(i,k,4),std(i,k,2),std(i,k,4) 
       endif
      enddo
     endif
     if(index2_norm(i) >=4.0) then
      write(76,105)  stdid(i),i,cid(i,1),cid(i,2),index2_norm(i)
     endif
 enddo 
        
        
          


105 format(2x,a8,i8,3f8.2)
100 format(2x,7f8.2,i3)
110 format(2x,i8,4f8.2)
120 format(2x,8f8.2)

deallocate(tmp2)

!       Print *,'w_selet:',dtype,n_sup,n_bias,n_rej_dir,n_rej_sp,n_other

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


   deallocate(stdid,cid,tdata_no2,n_tdata,tdata_no3)
   deallocate(std,std_no,rmean,xstas,break,xmed,smad)
   deallocate(index,index_rejsp,index_rejdir,index_other,index_qly,n2_tdata)
   deallocate(index2,index2_rejsp,index2_rejdir,index2_other,index2_qly,index2_norm)
     
     return
     end

