!   the program  is read the data from output of read_data.f90, then calculate 
!   statistics.  This program combining with read_data to have same function 
!   with read_data_stas 
!   stdid: station id 
!   cid:  station position and station and data type, cid(1): longitude
!    cid(2): latitude,cid(3): station height,cid(4):station type(fixed or moving)
!    cid(5): instrument type, the statis excludes the o-b greater than 15 
!    tdata(i,1,time): o-b,tdata(i,2,time): o-a,tdata(i,3,time): variational qc weight at first
!    iteration,tdata(i,4,time): variational qc weight at last,tdata(i,5,time):observation
!   n_tdata:  the count of missing observations

      subroutine stas_sfc(dtype,yr,mon,day,hr,shr,iqcflg)

   real(4),allocatable,dimension(:,:) :: tmp,tmp20,tmp22,tmp24,tmp26,tmp28,tmp30
    real(4),allocatable,dimension(:) :: tmp2

    character(len=3) :: mon
    character(len=4) :: shr,yr
    character(len=2) :: day,hr
    
!    character(len=8),dimension(35000) ::  stdid
!    real(4), dimension(35000,6) :: cid
!    real(4), dimension(35000,6,450) :: cid_time           !! for non fixed station 
!    real(8),dimension(35000,5,450) :: tdata
!    real(4),dimension(35000,2,450) :: tdata_no
!    real(4),dimension(35000,450) :: tdata_no3
!    integer, dimension(35000,2) :: n_stas
!    real(4),dimension(35000,3) :: tdata_no2
!    integer, dimension(35000) :: n_tdata

   real(8),dimension(450) :: xtmp,xtmp2 

!    real(8),dimension(35000,5) :: std,std_no,rmean,break
!    real(8),dimension(35000,2) :: xmed,xb,sb,smad

    character(len=8),allocatable,dimension(:) :: stdid,stdid_tmp
    real(4),allocatable,dimension(:,:,:) :: tdata
    real(8),allocatable,dimension(:,:) :: std,std_no,rmean,break,xmed,xb,sb,smad 
    real(8),allocatable,dimension(:) :: stdobs,stdobs_no,rmeanobs          
    real(4),allocatable,dimension(:,:,:) :: tdata_no,cid_time,tdata_tmp
    real(4),allocatable,dimension(:,:) :: cid,tdata_no3,tdata_no2
    integer,allocatable,dimension(:,:) :: n_stas
    integer,allocatable,dimension(:) :: n_tdata,itmp
   
   integer,dimension(4) :: iqcflg
     
    real(8) xmed1,xmed2,xmad1,xmad2,xb1,sb1,smad1

    real(4),dimension(1) :: plevel
    character(5) dtype,dtype2
    character*1 ddtype,ddtype2
    
    character(8)  stid,stationid 
    real(4) rlat,rlon,xlat0,xlon0,rtim,nlev0,nflag0

    character(50) infile,outfile,outfile1
    integer i_break

    real(8) rmiss
    real(4) rmiss_4
    integer lunin

 data rmiss/-999.0/ 
 data rmiss_4/-999.0/ 
 data plevel /1000./

 data lunin/21/

 

    nflag=1
  rtim=0.0
  nflg0=0
  xlat0=0.0
  xlon0=0.0
  nlev0=0
  stid='        '

      if ( trim(dtype) == 'ps120' ) then
         i_break=14
      else
        i_break=28
      endif
 
  ddtype=trim(dtype(1:1))

   infile=trim(dtype)//'_out'

   open(lunin,file=infile,form='unformatted')

   rewind(lunin)

   read(lunin) ndata1,nread,n

   print *,'ndata1,nread,n=',ndata1,nread,n

   if( n /= 5 ) then
     print *, 'wrong number for n, should be 5 ',n
     stop
    end if

   allocate(stdid(ndata1),cid(ndata1,6),cid_time(ndata1,6,nread),tdata(ndata1,5,nread))
   allocate(tdata_no(ndata1,2,nread),tdata_no2(ndata1,3),tdata_no3(ndata1,nread))
   allocate(n_tdata(ndata1),n_stas(ndata1,2))
   allocate(std(ndata1,5),std_no(ndata1,5),rmean(ndata1,5),break(ndata1,5))
   allocate(stdobs(ndata1),stdobs_no(ndata1),rmeanobs(ndata1))
   allocate(xmed(ndata1,2),xb(ndata1,2),sb(ndata1,2),smad(ndata1,2))

    stdid='        '
    tdata_no=0.0
    tdata=0.0
    n_tdata=0
    cid=0.0
 



   read(lunin) stdid(1:ndata1),cid(1:ndata1,:) 
    read(lunin) cid_time(1:ndata1,:,1:nread)
    read(lunin) tdata(1:ndata1,:,1:nread)
    read(lunin) tdata_no(1:ndata1,:,1:nread) 

    print *, cid(1,1),cid(1,2),cid(1,3),cid(1,4),cid(1,5),cid(1,6)
    print *, tdata(10,1,3),tdata(10,2,3),tdata(10,3,3),tdata(10,4,3),tdata(10,5,3)
    print *, tdata_no(10,1,3),tdata_no(10,2,3),tdata_no(10,1,4),tdata_no(10,2,4)

!    write(99,105) (tdata(16,5,i),i=1,nread) 
!    write(99,105) (tdata(11,5,i),i=1,nread) 
!    write(99,105) (tdata(76,5,i),i=1,nread) 
!105 format(8e14.6)

!    close(99)

    print *,ndata1,nread

    n_miss_crit=0
    if(trim(dtype) == 'ps120' ) then
      n_miss_crit=nread/6
   else
      n_miss_crit=nread/3
   endif

   print *,'n_miss_crit=',n_miss_crit

   tdata_no2=0.0
   tdata_no3=0.0

   do i=1,ndata1
   do it=1,nread
    if(tdata_no(i,1,it) >=0.0 .and. tdata_no(i,2,it) >=0.0) then
     tdata_no2(i,1)=tdata_no2(i,1)+tdata_no(i,1,it)+tdata_no(i,2,it) 
     tdata_no2(i,2)=tdata_no2(i,2)+tdata_no(i,2,it)
    endif
    if(tdata_no(i,1,it) >=1.0 .or. tdata_no(i,2,it) >=1.0) then
      tdata_no3(i,it) =1.0
    endif
    
   enddo
   enddo
   do i=1,ndata1
     if(tdata_no2(i,1) >=1.0) then
      tdata_no2(i,3) = tdata_no2(i,2)/tdata_no2(i,1) 
     else
       tdata_no2(i,3) =0.0
     endif
!   print *,'stas_sfc:',i,tdata_no2(i,1),tdata_no2(i,2),tdata_no2(i,3)
   enddo


   std=0.0
   stdobs=0.0
   rmean=0.0
   rmeanobs=0.0
   std_no=0.0
   stdobs_no=0.0
   do i=1,ndata1
   do j=1,5
   do it=1,nread
    if(tdata(i,j,it) >-90.0) then
      std_no(i,j)=std_no(i,j)+1.0
      rmean(i,j)=rmean(i,j)+tdata(i,j,it)
    endif
  enddo
  enddo
  enddo

  do i=1,ndata1
  do j=1,5
   if(std_no(i,j) >=1.0) then
     rmean(i,j)=rmean(i,j)/std_no(i,j)
   else
     rmean(i,j)=rmiss
   endif
  enddo
  enddo




  do i=1,ndata1
   do j=1,5
   do it=1,nread
    if(tdata(i,j,it) >-90.0 .and. rmean(i,j) >-99.0) then
      std(i,j)=std(i,j)+(tdata(i,j,it)-rmean(i,j))*(tdata(i,j,it)-rmean(i,j))
    endif
  enddo
  enddo
  enddo

  do i=1,ndata1
  do j=1,5
   if(std_no(i,j) >1.0 ) then
      std(i,j)=sqrt(std(i,j)/std_no(i,j))
   else if (std_no(i,j)  ==1.0) then 
     std(i,j)=0.0
   else if (std_no(i,j) <1.0) then
     std(i,j)=rmiss
   endif
  enddo
  enddo

istart=nread-20                   ! for katest 5 days observation, if constant, rejected
  do i=1,ndata1
  do it=istart,nread
    if(tdata(i,5,it) >-90.0) then
       rmeanobs(i)=rmeanobs(i)+tdata(i,5,it)
       stdobs_no(i)=stdobs_no(i)+1.0
    endif
  enddo
  enddo

  do i=1,ndata1
     if(stdobs_no(i) >1.0) then
        rmeanobs(i)=rmeanobs(i)/stdobs_no(i)
     else
        rmeanobs(i)=rmiss
     endif
  enddo

  do i=1,ndata1
   do it=istart,nread
    if(tdata(i,5,it) >-90.0 .and. rmeanobs(i) >-99.0) then
      stdobs(i)=stdobs(i)+(tdata(i,5,it)-rmeanobs(i))*(tdata(i,5,it)-rmeanobs(i))
!      if(stdid(i) == '46725') then
!         print * ,'stas_sfc:',tdata(i,5,it),stdobs(i),rmeanobs(i)
!      endif
    endif
  enddo
  enddo


   do i=1,ndata1
     if(trim(stdid(i)) == '83768') then
       write (99,300) (tdata(i,1,j),j=1,nread)
      write (99,*)
       write (99,301) rmean(i,1),std(i,1)
     endif
  enddo

300 format (10f12.2)
301 format (2f12.2)




  do i=1,ndata1
   if(stdobs_no(i) >1.0 ) then
      stdobs(i)=sqrt(stdobs(i)/stdobs_no(i))
   else if (stdobs_no(i)  <1.0) then 
     stdobs(i)=rmiss
   else if (stdobs_no(i) ==1.0) then
     stdobs(i)=0.0
   endif
  enddo

   n_stas=0
   xmed=rmiss
   smad=rmiss
   break=rmiss
   
   do i=1,ndata1
    do it=1,nread
      if(abs(tdata(i,1,it)) < 15.0) then
        n_stas(i,1)=n_stas(i,1)+1
        xtmp(n_stas(i,1))=tdata(i,1,it)
      endif
      if(abs(tdata(i,2,it))< 15.0) then
        n_stas(i,2)=n_stas(i,2)+1
        xtmp2(n_stas(i,2))=tdata(i,2,it)
      endif
    enddo
     nn1=n_stas(i,1)
     nn2=n_stas(i,2)
!     print *,i,'nn1=',nn1,'nn2=',nn2
    if(nn1 >=5) then
!     if(i ==74) then
!        write(6,150) (xtmp(jj),jj=1,nn1)
!     endif
      call biweight(nn1,xtmp,xmed1,xb1,sb1,smad1)
      xmed(i,1)=xmed1
      xb(i,1)=xb1
      sb(i,1)=sb1
      smad(i,1)=smad1
    endif
    ii_break=2*i_break
    if( nn1 >=ii_break) then
      call breakpoint(nn1,i_break,xtmp,xmed1,xmed2,xmad1,xmad2)
      break(i,1)=xmed1
      break(i,2)=xmed2
      break(i,3)=xmad1
      break(i,4)=xmad2
    endif 
    if(nn2 >=5) then
      call biweight(nn2,xtmp2,xmed1,xb1,sb1,smad1)
      xmed(i,2)=xmed1
      xb(i,2)=xb1
      sb(i,2)=sb1
      smad(i,2)=smad1
!      print *,xmed(i,2),xb(i,1),sb(i,2),smad(i,2)
    endif
   enddo
     
150 format(10f9.2) 

!    print *, tdata_no(1,1,1),tdata_no(2,1,1),tdata_no(3,1,1),tdata_no(4,1,1)
!    print *, tdata_no(1,2,1),tdata_no(2,2,1),tdata_no(3,2,1),tdata_no(4,2,1)
!    print *,'std:',std(1,1),std(2,1),std(3,1),std(4,1),std(5,1),std(6,1),std(7,1)
    print *,'rmean:',rmean(11,1),rmean(11,2),rmean(11,3),rmean(11,4),rmean(11,5),rmean(6,5),rmean(7,5)


!!   To filter stations without records for the past one month
!!   To filter stations with most records rejected by GSI gross check 

      outfile=trim(dtype)//'_discnt_list'
      open(70,file=outfile,form='formatted')

   n_qlyrej=0
   n_discnt=0
   istart=nread-n_miss_crit-1
    do i=1, ndata1
         do  it=istart,nread
!           if(abs(tdata(i,1,it)) <900.0) then
            if(tdata_no3(i,it) >0.0) then
              exit
           else
            n_tdata(i)=n_tdata(i)+1
           endif
         enddo
       if(n_tdata(i) == n_miss_crit) then
         n_discnt=n_discnt+1
         write(70,100) stdid(i),i,cid(i,1),cid(i,2),std_no(i,1),rmean(i,1),rmean(i,2),&
                       std(i,1),std(i,2),tdata_no2(i,1),tdata_no2(i,2),iqcflg(1)
         write(70,110) xmed(i,1),xmed(i,2),xb(i,1),xb(i,2),sb(i,1),sb(i,2),smad(i,1),smad(i,2)
         write(70,120) n_tdata(i),break(i,1), break(i,2),break(i,3), break(i,4) 
       endif
    enddo

100 format(a8,i8,9f8.2,i3)
110 format(16x,8f8.2)
120 format(16x,i8,4f8.2)

     close(70)

      nstation=0
      allocate(tmp(ndata1,6),tmp22(ndata1,3),tmp24(ndata1,nread),stdid_tmp(ndata1),itmp(ndata1))

!  check out the station could have almost constant observations
      outfile=trim(dtype)//'consobs_list'
      open(71,file=outfile,form='formatted')
      n_const=0

       do i=1, ndata1
          stationid=stdid(i)
          ddtype2=trim(stationid(8:8))
          if(trim(dtype) == 'ps188' .and. trim(ddtype2) == 'x' .and. rmean(i,1) >-90.0) then
             n_const=n_const+1
             write(71,100) stdid(i),i,cid(i,1),cid(i,2),std_no(i,1),rmean(i,1),rmean(i,2),&
                       std(i,1),std(i,2),tdata_no2(i,1),tdata_no2(i,2),iqcflg(2)
             write(71,110) xmed(i,1),xmed(i,2),xb(i,1),xb(i,2),sb(i,1),sb(i,2),smad(i,1),smad(i,2)
             write(71,120) n_tdata(i),break(i,1), break(i,2),break(i,3), break(i,4)
          else if(rmean(i,1) >-900.0) then
             if(stdobs(i) <=0.05 .and. stdobs_no(i) >=5.0 .and. rmeanobs(i) >-900.0) cid(i,6)=-100.0
             nstation=nstation+1
             stdid_tmp(nstation)=stdid(i)
             tmp(nstation,1:6)=cid(i,1:6)
             itmp(nstation)=n_tdata(i)
             tmp22(nstation,1:3)=tdata_no2(i,1:3)
             tmp24(nstation,1:nread)=tdata_no3(i,1:nread)
          endif
          if(stdobs(i) <=0.05 .and. rmeanobs(i) >-90.0 .and. stdobs_no(i) >=5.0 ) then
             n_const=n_const+1
             write(71,100) stdid(i),i,cid(i,1),cid(i,2),std_no(i,1),rmean(i,1),rmean(i,2),&
                       std(i,1),std(i,2),tdata_no2(i,1),tdata_no2(i,2),iqcflg(2)
             write(71,110) xmed(i,1),xmed(i,2),xb(i,1),xb(i,2),sb(i,1),sb(i,2),smad(i,1),smad(i,2)
             write(71,120) n_tdata(i),break(i,1), break(i,2),break(i,3),break(i,4)
             write(71,140) stdobs(i),rmeanobs(i),stdobs_no(i) 
          endif
       enddo


130 format(a8,2f8.2)
140 format(3f8.2)
         print *,'n_const=',n_const
            
           
      close(71)

    !!  get suspecious station list

!    if (trim(ddtype) == 'p')  then
!     call ps_select(dtype,stdid,cid,std,std_no,tdata_no2,n_tdata,rmean,&
!                    break,xmed,xb,sb,smad,30000,ndata1,rmiss_4)
!    else if(trim(ddtype) == 'q') then 
!     call q_select_sfc(dtype,stdid,cid,std,std_no,tdata_no2,n_tdata,rmean,&
!                       break,xmed,xb,sb,smad,30000,ndata1,rmiss_4)
!    else if(trim(ddtype) == 't') then
!     call t_select_sfc(dtype,stdid,cid,std,std_no,tdata_no2,n_tdata,rmean,&
!                       break,xmed,xb,sb,smad,30000,ndata1,rmiss_4)
!   endif
 
       
    
  outfile=trim(dtype)//'_station'   

    open(10,file=outfile,form='unformatted')

     write(10) nstation,nread,n_miss_crit 
     write(10) stdid_tmp(1:nstation),tmp(1:nstation,:) 
     write(10) itmp(1:nstation)
     write(10) tmp22(1:nstation,:)
     write(10) tmp24(1:nstation,1:nread)
  

close(10)
   deallocate(tmp24)
  allocate(tmp26(nstation,5),tmp20(nstation,5),tmp24(nstation,5),tmp28(nstation,5),tmp30(nstation,5))
   

   print *, 'stas_sfc: starting to write stas files,ndata1=',ndata1
  outfile=trim(dtype)//'_stas'   

 open(20,file=outfile,form='unformatted')


   nstation2=0
   do i=1,ndata1
     if(trim(dtype) == 'ps188') then
        stationid=stdid(i)
        ddtype2=trim(stationid(8:8))
        if(rmean(i,1) >-900.0 .and. trim(ddtype2) /= 'x') then
          nstation2=nstation2+1
          tmp26(nstation2,1:5)=rmean(i,1:5)
          tmp20(nstation2,1:5)=std(i,1:5)
          tmp24(nstation2,1:2)=xmed(i,1:2)
          tmp24(nstation2,3:4)=smad(i,1:2)
          tmp24(nstation2,5)=xb(i,1)
          tmp28(nstation2,1:5)=break(i,1:5)
          tmp30(nstation2,1:5)=std_no(i,1:5)
        endif
     else if(rmean(i,1) >-900.0) then
        nstation2=nstation2+1
        tmp26(nstation2,1:5)=rmean(i,1:5)
        tmp20(nstation2,1:5)=std(i,1:5)
        tmp24(nstation2,1:2)=xmed(i,1:2)
        tmp24(nstation2,3:4)=smad(i,1:2)
        tmp24(nstation2,5)=xb(i,1)
        tmp28(nstation2,1:5)=break(i,1:5)
        tmp30(nstation2,1:5)=std_no(i,1:5)
     endif
  enddo

    print *,(tmp26(i,1),i=1,6)

   if(nstation2 == nstation) then
     write(20) tmp26
     write(20) tmp20
     write(20) tmp24
     write(20) tmp28
     write(20) tmp30
   else
     print *,'nstation=',nstation,nstation2     
     stop
   endif

close(20)

     print *,'nstation=',nstation,nstation2 

  call stas_hist_sfc(dtype,stdid_tmp,tmp,tmp26,tmp20,ndata1,nstation,rmiss_4)

   print *,'start to call creat_time_sfc'

  call creat_ctl_time_sfc(dtype,ndata1,nstation,5,nread,stdid_tmp,tmp,tmp20,tmp26,tmp24,&
                      yr,mon,day,hr,shr,rmiss_4)
  call creat_ctl_stas(dtype,1,ndata1,nstation,stdid_tmp,tmp,plevel,rmiss_4)
  call creat_ctl_station(dtype,1,plevel,rmiss_4)

   print *, 'stas_sfc: finish to write stas files'
  outfile=trim(dtype)//'_stas_station'   
 open (25,file=outfile,form='unformatted')

allocate(tmp2(12))

 do i=1,nstation
       stid=stdid_tmp(i)
       rlat=tmp(i,2)
       rlon=tmp(i,1)
    write(25) stid,rlat,rlon,rtim,1,1
      tmp2(1)=tmp26(i,1)
      tmp2(2)=tmp26(i,2)
      tmp2(3)=tmp26(i,5)
      tmp2(4)=tmp20(i,1)
      tmp2(5)=tmp20(i,2)
      tmp2(6)=tmp20(i,5)    !obs
      tmp2(7)=tmp24(i,1)
      tmp2(8)=tmp24(i,2)
      tmp2(9)=tmp24(i,3)
      tmp2(10)=tmp24(i,4)
      tmp2(11)=tmp30(i,1)
      tmp2(12)=tmp22(i,2)
      write(25) tmp2
  enddo

!!  the end of file
     stid='        '
     write(25) stid,xlat0,xlon0,rtim,nlev0,nflag0
           
close(25)

            
!   write time series files
  outfile=trim(dtype)//'_time'   

  open (30,file=outfile,form='unformatted')         


  allocate(tdata_tmp(ndata1,7,nread))
  
   nstation3=0
   do i=1,ndata1
     if(trim(dtype) == 'ps188') then
        stationid=stdid(i)
        ddtype2=trim(stationid(8:8))
        if(rmean(i,1) >-900.0 .and. trim(ddtype2) /= 'x') then
          nstation3=nstation3+1
          tdata_tmp(nstation3,1:5,1:nread)=tdata(i,1:5,1:nread)
          tdata_tmp(nstation3,6:7,1:nread)=tdata_no(i,1:2,1:nread)
        endif
     else if(rmean(i,1) >-900.0) then
        nstation3=nstation3+1
        tdata_tmp(nstation3,1:5,1:nread)=tdata(i,1:5,1:nread)
        tdata_tmp(nstation3,6:7,1:nread)=tdata_no(i,1:2,1:nread)
     endif
   enddo

    close(99)
   if(nstation3 /= nstation) then
     print *,'nstation,nstation3=',nstation,nstation3 
     stop
   endif

  deallocate(tmp2)
  allocate(tmp2(nstation))

  do it=1,nread
    tmp2(1:nstation)=tdata_tmp(1:nstation,1,it) 
    write(30) tmp2 
    tmp2(1:nstation)=tdata_tmp(1:nstation,2,it)
    write(30) tmp2 
    tmp2(1:nstation)=tdata_tmp(1:nstation,3,it)
    write(30) tmp2 
    tmp2(1:nstation)=tdata_tmp(1:nstation,4,it)
    write(30)  tmp2 
    tmp2(1:nstation)=tdata_tmp(1:nstation,5,it)
    write(30)  tmp2 
    tmp2(1:nstation)=tdata_tmp(1:nstation,6,it)
    write(30) tmp2     
    tmp2(1:nstation)=tdata_tmp(1:nstation,7,it)
    write(30) tmp2     
enddo

   close(30)

!  deallocate(tmp2,rmean,tdata_no,tdata,tdata_tmp,stdid)
  deallocate(stdid,stdid_tmp,tdata,std,std_no,rmean,break,xmed,xb,sb,smad)
  deallocate(stdobs,stdobs_no,rmeanobs,tdata_no,cid_time,tdata_tmp)
 deallocate(cid,tdata_no3,tdata_no2,n_stas,n_tdata,itmp)
   deallocate(tmp,tmp2,tmp20,tmp22,tmp24,tmp26,tmp30,tmp28)


    print *,'stas_sfc:',ndata1,nstation,nread,n_qlyrej,n_discnt

   return 
   end
    
