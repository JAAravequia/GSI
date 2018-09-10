!   the program read the data 


!  stdid:  station ID, dimension is total station number for the type
!  cid: get station infomation: 1: logitude,2 latitude
!  3: station elevation, 4: DATA DUMP REPORT TYPE
!  5: instrument type, 6: EPBUFR REPORT TYPE
!  tdata(d1,d2,d3,d4): d1 is the station number 
!   d2 is vertical level, total about 43 level, the detail of level can be seen in read prepbufr files 
!   d3 is the statistic elements: 1:O-B,2: O-A, 3:variational qc weight at begining, 4:variation qc weight 
!   at the end, 5: observation values. d4: count in time
!   tdata_no(d1,d2,d3,d4): the data sccount, d1:  total station number, d2:vertical level,d3: data
!  count, 1: used data counter in GSI, 2:  rejected by variaous qc. d4: count in time         
!  

    subroutine stas(dtype,nk,yr,mon,day,hr,shr)

    real(4),allocatable,dimension(:) :: tmp2
    real(4),allocatable,dimension(:,:) :: tmp
    

    character(len=3) :: mon
    character(len=4) :: shr,yr
    character(len=2) :: day,hr
!    character(len=8),dimension(2000) ::  stdid
!    real(4), dimension(2000,6) :: cid
!    real(4), dimension(2000,6,380) :: cid_time           !! for non fixed station 
!    real(8),dimension(2000,nk,5,380) :: tdata
!    real(4),dimension(2000,nk,2,380) :: tdata_no
!    real(4),dimension(2000,nk,380) :: tdata_no3
!    real(4),dimension(2000,nk,3) :: tdata_no2
!    integer,dimension(2000,nk) :: n_tdata
!    integer,dimension(2000,nk,2) :: n_stas
!    real(8),dimension(2000,nk,5) :: std,std_no,rmean,break
!    real(8),dimension(2000,nk,2) :: xmed,xb,sb,smad 

    character(len=8),allocatable,dimension(:) :: stdid
    real(4),allocatable,dimension(:,:,:,:) :: tdata
    real(8),allocatable,dimension(:,:,:) :: std,std_no,rmean,break,xmed,xb,sb,smad 
    real(4),allocatable,dimension(:,:) :: cid
    real(4),allocatable,dimension(:,:,:) :: cid_time,tdata_no3,tdata_no2
    real(4),allocatable,dimension(:,:,:,:) :: tdata_no
    integer,allocatable,dimension(:,:,:) :: n_stas 
    integer,allocatable,dimension(:,:) :: n_tdata 

    character(5) dtype
    character(1) ddtype
    real(4),dimension(43) :: plevel
    real(8),dimension(450) :: xtmp,xtmp2
    real(4) rlat,rlon,xlat0,xlon0,rtim,nlev0,nflag0
    real(8) xb1,sb1,smad1,xmed1,xmed2,xmad1,xmad2
    
    character(8)  stid 

    character(100) cfile
    character(50) fname,infile,fileo,outfile,outfile1

    real(8) rmiss
    real(4) rmiss_4

    integer i_break

  data plevel/1000.,975.,950.,925.,900.,875.,850.,825.,800.,775.,750.,725.,700.,675.,650.,625.,600.,575.,550.,525.,500.,475.,450.,425.,400.,375.,350.,325.,300.,275.,250.,225.,200.,175.,150.,125.,100.,70.,50.,30.,20.,10.,5./

! data dtype /'t120'/
 data rmiss/-999.0/ 
 data rmiss_4/-999.0/ 
 data lunin/51/ 

!    namelist /input/dtype
!    read(5,input)
!    write(6,*)' User input below'
!    write(6,input)

     print *,dtype,nk,iyr,imon,iday,ihr
     ddtype=trim(dtype(1:1))
!  for a week data

      if ( trim(dtype) == 'q120' .or. trim(dtype) == 't120' ) then
         i_break=14
      else
        i_break=28
      endif

     if (nk >43 ) then
       print *, 'nk is too big ',nk
       stop
     endif

    nread=0
    rtim=0.0
  nflg0=0
  xlat0=0.0
  xlon0=0.0
  nlev0=0
  stid='        '

  infile=trim(dtype)//'_out'

   open(lunin,file=infile,form='unformatted')

   rewind(lunin)

   read(lunin) ndata1,nread,n

   print *,'ndata1,nread,n=',ndata1,nread,n

    tdata_no=rmiss_4
    tdata=rmiss
    tdata_no2=0.0
    tdata_no3=0.0

    if( n /= 5 ) then
     print *, 'wrong number for n, should be 5 ',n
     stop
    end if

   allocate(stdid(ndata1),cid(ndata1,6),cid_time(ndata1,6,nread),tdata(ndata1,nk,5,nread))
   allocate(tdata_no(ndata1,nk,2,nread),tdata_no2(ndata1,nk,3),tdata_no3(ndata1,nk,nread))
   allocate(n_tdata(ndata1,nk),n_stas(ndata1,nk,2))
   allocate(std(ndata1,nk,5),std_no(ndata1,nk,5),rmean(ndata1,nk,5),break(ndata1,nk,5))
   allocate(xmed(ndata1,nk,2),xb(ndata1,nk,2),sb(ndata1,nk,2),smad(ndata1,nk,2))

   read(lunin) stdid(1:ndata1),cid(1:ndata1,:)
    read(lunin) cid_time(1:ndata1,:,1:nread)
    read(lunin) tdata(1:ndata1,:,:,1:nread)
    read(lunin) tdata_no(1:ndata1,:,:,1:nread)

   
    write(6,500) (tdata(5,k,1,5),k=1,nk)
500 format(10f8.2)

!  define criteria for the station missing for past month

   n_miss_crit=0
   if(trim(dtype) == 'q120' .or. trim(dtype) == 't120' ) then
      n_miss_crit=nread/6
   else  
      n_miss_crit=nread/3
   endif

   print *,'n_miss_crit=',n_miss_crit

 do i=1,ndata1
   do it=1,nread
   do k=1,nk
    if(tdata_no(i,k,1,it) >=0.0 .and. tdata_no(i,k,2,it) >=0.0) then
     tdata_no2(i,k,1)=tdata_no2(i,k,1)+tdata_no(i,k,1,it)+tdata_no(i,k,2,it)
     tdata_no2(i,k,2)=tdata_no2(i,k,2)+tdata_no(i,k,2,it)
    endif
    if(tdata_no(i,k,1,it) >=1.0 .or. tdata_no(i,k,2,it) >=1.0) then
      tdata_no3(i,k,it) =1.0
    endif
   enddo
   enddo
   enddo

do i=1,ndata1
 do k=1,nk
     if(tdata_no2(i,k,1) >=1.0) then
      tdata_no2(i,k,3) = tdata_no2(i,k,2)/tdata_no2(i,k,1)
     else
       tdata_no2(i,k,3) =0.0
     endif
   enddo
   enddo



  
   std=0.0
   rmean=0.0
   std_no=0.0
   do i=1,ndata1
   do j=1,5
   do k=1,nk
   do it=1,nread
    if(tdata(i,k,j,it) >-90.0) then
      std_no(i,k,j)=std_no(i,k,j)+1.0
!      std(i,k,j)=std(i,k,j)+tdata(i,k,j,it)*tdata(i,k,j,it)
      rmean(i,k,j)=rmean(i,k,j)+tdata(i,k,j,it)
    endif
  enddo
  enddo
  enddo
  enddo

  do i=1,ndata1
  do j=1,5
  do k=1,nk
     if(std_no(i,k,j)>=1.0) then
        rmean(i,k,j)=rmean(i,k,j)/std_no(i,k,j)
     else
        rmean(i,k,j)=rmiss
     endif
  enddo
  enddo
  enddo

  do i=1,ndata1
   do j=1,5
   do k=1,nk
   do it=1,nread
    if(tdata(i,k,j,it) >-90.0 .and. rmean(i,k,j) >-90.0) then
!      std_no(i,k,j)=std_no(i,k,j)+1.0
      std(i,k,j)=std(i,k,j)+(tdata(i,k,j,it)-rmean(i,k,j))*(tdata(i,k,j,it)-rmean(i,k,j))
    endif
  enddo
  enddo
  enddo
  enddo


  do i=1,ndata1
  do j=1,5
  do k=1,nk
   if(std_no(i,k,j) >1.0 .and. std(i,k,j) >0.0) then
     std(i,k,j)=sqrt(std(i,k,j)/std_no(i,k,j))
   else if(std_no(i,k,j) >1.0 .and. std(i,k,j) <=0.0) then
     std(i,k,j)=0.0
   else if ( std_no(i,k,j) ==1.0) then
     std(i,k,j)=0.0
   else
     std(i,k,j)=rmiss
   endif
  enddo
  enddo
  enddo

! calculate the median and other robster statistics

 n_stas=0
   xmed=-999.0
   smad=-999.0
   break=-999.0

    do i=1,ndata1
    do k=1,nk
    do it=1,nread
      if(abs(tdata(i,k,1,it)) < 90.0) then
        n_stas(i,k,1)=n_stas(i,k,1)+1
        xtmp(n_stas(i,k,1))=tdata(i,k,1,it)
      endif
      if(abs(tdata(i,k,2,it))< 90.0) then
        n_stas(i,k,2)=n_stas(i,k,2)+1
        xtmp2(n_stas(i,k,2))=tdata(i,k,2,it)
      endif
    enddo
     nn1=n_stas(i,k,1)
     nn2=n_stas(i,k,2)
!     print *,i,'nn1=',nn1,'nn2=',nn2
    if(nn1 >=5) then
!     if(i ==74) then
!        write(6,150) (xtmp(jj),jj=1,nn1)
!     endif
      call biweight(nn1,xtmp,xmed1,xb1,sb1,smad1)
      xmed(i,k,1)=xmed1
      xb(i,k,1)=xb1
      sb(i,k,1)=sb1
      smad(i,k,1)=smad1
endif
    ii_break=i_break*2
    if( nn1 >=ii_break) then
      call breakpoint(nn1,i_break,xtmp,xmed1,xmed2,xmad1,xmad2)
      break(i,k,1)=xmed1
      break(i,k,2)=xmed2
      break(i,k,3)=xmad1
      break(i,k,4)=xmad2
    endif
    if(nn2 >=5) then
      call biweight(nn2,xtmp2,xmed1,xb1,sb1,smad1)
      xmed(i,k,2)=xmed1
      xb(i,k,2)=xb1
      sb(i,k,2)=sb1
      smad(i,k,2)=smad1
!      print *,xmed(i,2),xb(i,1),sb(i,2),smad(i,2)
    endif
   enddo
   enddo

150 format(10f9.2)


!!  make grads control file

  call creat_ctl_time(dtype,nk,ndata1,ndata1,nread,stdid,cid,plevel,yr,&
                  mon,day,hr,shr,rmiss_4)
   call creat_ctl_stas(dtype,nk,ndata1,ndata1,stdid,cid,plevel,rmiss_4)
   call creat_ctl_station(dtype,nk,plevel,rmiss_4)

!!!  get histogram statistics

     call stas_hist(dtype,stdid,cid,rmean,std,ndata1,ndata1,plevel,nk,rmiss_4)

!!   To filter stations without records for the past one month
!!   To filter stations with most records rejected by GSI gross check

      print *,'stas: starting to filter bad data'

   outfile=trim(dtype)//'_discnt_list'

      open(70,file=outfile,form='formatted')

    n_tdata=0
    n_dis=0
   istart=nread-n_miss_crit
    do i=1, ndata1
        do k=1,nk
         do  it=istart,nread
!           if(abs(tdata(i,k,1,it)) <900.0) then
            if(tdata_no3(i,k,it) >0.0) then
              exit
           else
            n_tdata(i,k)=n_tdata(i,k)+1
           endif
         enddo
        enddo
         kcount=0
      do k=1,nk
        if(n_tdata(i,k) == n_miss_crit) then          !!! get account vertical levels with
          kcount=kcount+1                     !!! missing data in recent 30 days 
        endif
      enddo
       if (kcount == nk ) then
         n_dis=n_dis+1
         write(70,100) stdid(i),i,cid(i,1),cid(i,2)
       endif
    enddo

100 format(a8,i8,2f8.2)
105 format(a8,i8,10f8.2)
110 format(16x,i4,8f8.2)
120 format(16x,4f8.2)

!              if( trim(ddtype) == 'q') then
!                 call q_select(dtype,stdid,cid,std,std_no,tdata_no2,n_tdata,rmean,break,xmed,xb,&
!                              sb,smad,plevel,2000,ndata1,nk,rmiss)
!              else if(trim(ddtype) == 't') then
!                 call t_select(dtype,stdid,cid,std,std_no,tdata_no2,n_tdata,rmean,break,xmed,xb,&
!                              sb,smad,plevel,2000,ndata1,nk,rmiss)
!              endif


    print *,'stas:',dtype,'n_dis=',n_dis
    

    fileo=trim(dtype)//'_station'
    open(31,file=fileo,form='unformatted')
     write(31) ndata1,nread,n_miss_crit
     write(31) stdid(1:ndata1)
     write(31) cid(1:ndata1,:) 
     write(31) n_tdata(1:ndata1,1:nk)
     write(31) tdata_no2(1:ndata1,1:nk,1:3)
     write(31) tdata_no3(1:ndata1,1:nk,1:nread)
  

!close(31)

! for vertical profile
     fileo=trim(dtype)//'_stas'


 open (20,file=fileo,form='unformatted')
  allocate(tmp(ndata1,5))
  do k=1,nk
    tmp(1:ndata1,:)=rmean(1:ndata1,k,:)
    write(20) tmp 
  enddo
  do k=1,nk
    tmp(1:ndata1,:)=std(1:ndata1,k,:)
    write(20) tmp 
  enddo
 
 do k=1,nk
   tmp(1:ndata1,1:2)=xmed(1:ndata1,k,:)
   tmp(1:ndata1,3:4)=smad(1:ndata1,k,:)
   tmp(1:ndata1,5)=xb(1:ndata1,k,1)
   write(20) tmp
 enddo

  do k=1,nk
   tmp(1:ndata1,:)=break(1:ndata1,k,:)
   write(20) tmp 
  enddo

  do k=1,nk
   tmp(1:ndata1,:)=std_no(1:ndata1,k,:)
    write(20) tmp
 enddo    
      deallocate(tmp)
close(20)


! for horizontal map
   fileo=trim(dtype)//'_stas_station'
 open (25,file=fileo,form='unformatted')

  allocate(tmp2(12))

 do i=1,ndata1
    stid=stdid(i)
    rlat=cid(i,2)
    rlon=cid(i,1)
   do k=1,nk
    write(25) stid,rlat,rlon,rtim,1,0
!    do k=1,nk
      if(k == nk .and.std_no(i,k,1) >0.0 ) print *,plevel(k),rmean(i,k,1),rmean(i,k,2),std(i,k,1),std(i,k,2),std_no(i,k,1)
      tmp2(1)=rmean(i,k,1)
      tmp2(2)=rmean(i,k,2)
      tmp2(3)=rmean(i,k,5)
      tmp2(4)=std(i,k,1)
      tmp2(5)=std(i,k,2)
      tmp2(6)=std(i,k,5)    !obs
      tmp2(7)=xmed(i,k,1)
      tmp2(8)=xmed(i,k,2)
      tmp2(9)=smad(i,k,1)
      tmp2(10)=smad(i,k,2)
      tmp2(11)=std_no(i,k,1)
      tmp2(12)=tdata_no2(i,k,2)
      write(25) plevel(k),tmp2
    enddo
  enddo

!  the end of file
     stid='        '
     write(25) stid,xlat0,xlon0,rtim,nlev0,nflag0

      deallocate(tmp2)
close(25)

            
!   write time series files

fileo=trim(dtype)//'_time'

  open (30,file=fileo,form='unformatted')         

   allocate (tmp2(ndata1))
  do it=1,nread

   do k=1,nk
    tmp2(1:ndata1)=tdata(1:ndata1,k,1,it)
    write(30) tmp2
   enddo
   do k=1,nk
    tmp2(1:ndata1)=tdata(1:ndata1,k,2,it)
    write(30) tmp2
   enddo
   do k=1,nk
    tmp2(1:ndata1)=tdata(1:ndata1,k,3,it)
    write(30) tmp2
   enddo
   do k=1,nk
    tmp2(1:ndata1)=tdata(1:ndata1,k,4,it)
    write(30) tmp2
   enddo
   do k=1,nk
    tmp2(1:ndata1)=tdata(1:ndata1,k,5,it)
    write(30) tmp2
   enddo
   do k=1,nk
    tmp2(1:ndata1)=tdata_no(1:ndata1,k,1,it)
    write(30) tmp2
   enddo
   do k=1,nk
    tmp2(1:ndata1)=tdata_no(1:ndata1,k,2,it)
    write(30) tmp2
   enddo
      
enddo

      deallocate(tmp2)
   close(30)

   fileo=trim(dtype)//'_station.list'

    open(50,file=fileo,form='formatted')

    do i=1,ndata1
       if(cid(i,5) >100000.00) cid(i,5)=-99.0
       icid=nint(cid(i,5))
       write(50,1000) stdid(i),cid(i,1),cid(i,2),icid
    enddo

1000 format(a8, 2f10.2,i10)

    deallocate(stdid,cid,cid_time,tdata,tdata_no,tdata_no3,tdata_no2,&
               std,std_no,rmean,break,xmed,xb,sb,smad,n_tdata,n_stas)
   return 
   end
   

