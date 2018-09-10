!   the program read the data 

!   array for data tdata items: 1: station data number; 2: vertical level
!   3: data items, 4: time 
!   data items: 1: uo-ub;2:uo-ua;3:variational qc weight (bac);4: variational
!   qc weight (ana);5:vo-vb;6:vo-va;7: uobs;8:vobs;9:directionO-directionB;  
!   10: directionO-directionA;11: speedO-speedB;12:speedO-speedB;13:direction
!   obs;14: speed obs

    subroutine stas_spdir(dtype,nk,yr,mon,day,hr,shr)

    real(4),allocatable,dimension(:) :: tmp2
    real(4),allocatable,dimension(:,:) :: tmp
    
    character(len=3) :: mon
    character(len=4) :: shr,yr
    character(len=2) :: day,hr

!    character(len=8),dimension(2000) ::  stdid
!    real(4), dimension(2000,6) :: cid
!    real(4), dimension(2000,6,400) :: cid_time           !! for non fixed station 
!    real(4),dimension(2000,nk,14,400) :: tdata
!    real(4),dimension(2000,nk,2,400) :: tdata_no
!    real(4),dimension(2000,nk,400) :: tdata_no3
!    real(4),dimension(2000,nk,3) :: tdata_no2
!    integer(4),dimension(2000,nk) :: n_tdata
!    real(8),dimension(2000,nk,6) :: std,std_no,rmean,break
!    real(8),dimension(2000,nk,2) :: xmed,xb,sb,smad 
!    real(8),dimension(2000,nk) ::  nom_std 
!    real(4),dimension(2000,nk) ::  lspno 

     character(len=8),allocatable,dimension(:) :: stdid
    real(4),allocatable,dimension(:,:,:,:) :: tdata
    real(8),allocatable,dimension(:,:,:) :: std,std_no,rmean,break,xmed,xb,sb,smad
    real(8),allocatable,dimension(:,:) :: nom_std 
    real(4),allocatable,dimension(:,:) :: cid,lspno
    real(4),allocatable,dimension(:,:,:) :: cid_time,tdata_no3,tdata_no2
    real(4),allocatable,dimension(:,:,:,:) :: tdata_no
    integer,allocatable,dimension(:,:,:) :: n_stas
    integer,allocatable,dimension(:,:) :: n_tdata


    real(4),dimension(43) :: plevel,all_lsp
    real(4),dimension(51) :: hist
    real(4),dimension(51,nk)   :: lsp_hist
    character(5) dtype
    

    real(8),dimension(450) :: xtmp,xtmp2
    real(8) xb1,sb1,smad1,xmed1,xmed2,xmad1,xmad2
    
    character(8)  stid 
    real(4) rlat,rlon,xlat0,xlon0,rtim,nlev0,nflag0

    character(100) cfile
    character(50) fname,infile,file1,outfile,outfile1

    real(8) rmiss
    real(4) rmiss_4
    integer i_break

  data plevel/1000.,975.,950.,925.,900.,875.,850.,825.,800.,775.,750.,725.,700.,675.,650.,625.,600.,575.,550.,525.,500.,475.,450.,425.,400.,375.,350.,325.,300.,275.,250.,225.,200.,175.,150.,125.,100.,70.,50.,30.,20.,10.,5./

 data rmiss/-999.0/ 
 data rmiss_4/-999.0/ 
 data lunin/51/ 

!    namelist /input/dtype
!    read(5,input)
!    write(6,*)' User input below'
!    write(6,input)

    print *, 'tas_spdir:nk=',nk
    nread=0
  rtim=0.0
  nflg0=0
  xlat0=0.0
  xlon0=0.0
  nlev0=0
  stid='        '

  if ( trim(dtype) == 'uv220' .or. trim(dtype) == 'uv221' ) then
         i_break=14
      else
        i_break=28
      endif


  infile=trim(dtype)//'_out'

   open(lunin,file=infile,form='unformatted')

   rewind(lunin)

   read(lunin) ndata1,nread,n

    print *, 'stas_spdir ',ndata1,nread,n

   if( n /= 14) then
     print *, 'wrong number for n, should be 14 ',n
     stop
    end if

   allocate(stdid(ndata1),cid(ndata1,6),cid_time(ndata1,6,nread),tdata(ndata1,nk,14,nread))
   allocate(tdata_no(ndata1,nk,2,nread),tdata_no2(ndata1,nk,3),tdata_no3(ndata1,nk,nread))
   allocate(n_tdata(ndata1,nk),n_stas(ndata1,nk,2))
   allocate(std(ndata1,nk,6),std_no(ndata1,nk,6),rmean(ndata1,nk,6),break(ndata1,nk,6))
   allocate(xmed(ndata1,nk,2),xb(ndata1,nk,2),sb(ndata1,nk,2),smad(ndata1,nk,2))
   allocate(nom_std(ndata1,nk),lspno(ndata1,nk))


   print *,ndata1,nread
    tdata_no=rmiss
    tdata=rmiss
    tdata_no2=0.0
    tdata_no3=0.0

   read(lunin) stdid(1:ndata1),cid(1:ndata1,:)
    read(lunin) cid_time(1:ndata1,:,1:nread)
    read(lunin) tdata(1:ndata1,:,:,1:nread)
    read(lunin) tdata_no(1:ndata1,:,:,1:nread)

   
    write(6,500) (tdata(5,4,k,5),k=9,14)
    write(6,500) (tdata(5,3,k,5),k=9,14)
    write(6,500) (tdata_no(5,k,1,1),k=1,10)
    write(6,500) (tdata_no(5,k,2,1),k=1,10)
500 format(10f8.2)

! calculate wind observed speed mean

     n_miss_crit=0
    if(trim(dtype) == 'uv220' ) then
      n_miss_crit=nread/6
   else
      n_miss_crit=nread/3
   endif

   
   lspno=0.0
   do i=1,ndata1
   do k=1,nk
   do iread=1,nread
       if(tdata(i,k,14,iread) >-300.0 .and. tdata(i,k,14,iread) <=0.3) then
           lspno(i,k)=lspno(i,k)+1.0
       endif
   enddo
   enddo
   enddo


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
!      print *,'stas_spdir,tdata_no2:',tdata_no2(i,k,1),tdata_no2(i,k,2),tdata_no2(i,k,3)
     else
       tdata_no2(i,k,3) =0.0
     endif
   enddo
   enddo





  
   std=0.0
   rmean=0.0
   std_no=0.0
   do i=1,ndata1
   do j=1,6
   do k=1,nk
   do it=1,nread
    jj=j+8
    if(tdata(i,k,jj,it) >-300.0) then
      std_no(i,k,j)=std_no(i,k,j)+1.0
!      std(i,k,j)=std(i,k,j)+tdata(i,k,jj,it)*tdata(i,k,jj,it)
      rmean(i,k,j)=rmean(i,k,j)+tdata(i,k,jj,it)
    endif
  enddo
  enddo
  enddo
  enddo

  do i=1,ndata1
  do j=1,6
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
   do j=1,6
   do k=1,nk
   do it=1,nread
     jj=j+8
    if(tdata(i,k,jj,it) >-300.0 .and. rmean(i,k,j) >-300.0) then
!      std_no(i,k,j)=std_no(i,k,j)+1.0
      std(i,k,j)=std(i,k,j)+(tdata(i,k,jj,it)-rmean(i,k,j))*(tdata(i,k,jj,it)-rmean(i,k,j))
    endif
  enddo
  enddo
  enddo
  enddo

  do i=1,ndata1
  do j=1,6
  do k=1,nk
   if(std_no(i,k,j) >1.0) then
     std(i,k,j)=sqrt(std(i,k,j)/std_no(i,k,j))
   else if ( std_no(i,k,j) ==1.0) then
     std(i,k,j)=0.0
   else
     std(i,k,j)=rmiss
   endif
  enddo
  enddo
  enddo

   print *, 'std,rmean'
   print *, std(3,3,1),std(3,3,3),std(3,4,1),std(3,4,3),std(5,3,1),std(5,3,3) 
   print *, rmean(3,3,1),rmean(3,3,3),rmean(3,4,1),rmean(3,4,3),rmean(5,3,1),rmean(5,3,3) 

! calculate normalized o-b speed standard deviation 

  nom_std=0.0
  do i=1,ndata1
  do k=1,nk
     if(rmean(i,k,6) >1.0)  then 
        nom_std(i,k)=std(i,k,6)/rmean(i,k,6)    
     else
        nom_std(i,k)=rmiss
     endif
  enddo
  enddo


! calculate the median and other robster statistics

 n_stas=0
   xmed=rmiss
   smad=rmiss
   break=rmiss

    do i=1,ndata1
    do k=1,nk
    do it=1,nread
      if(abs(tdata(i,k,9,it)) < 300.0) then    ! direction
        n_stas(i,k,1)=n_stas(i,k,1)+1
        xtmp(n_stas(i,k,1))=tdata(i,k,9,it)
      endif
      if(abs(tdata(i,k,11,it))< 900.0) then    ! speed
        n_stas(i,k,2)=n_stas(i,k,2)+1
        xtmp2(n_stas(i,k,2))=tdata(i,k,11,it)
      endif
    enddo
     nn1=n_stas(i,k,1)
     nn2=n_stas(i,k,2)
    if(nn1 >=5) then
      call biweight(nn1,xtmp,xmed1,xb1,sb1,smad1)
      xmed(i,k,1)=xmed1
      xb(i,k,1)=xb1
      sb(i,k,1)=sb1
      smad(i,k,1)=smad1
    endif
    ii_break=2*i_break
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

  call creat_ctl_time(dtype,nk,ndata1,ndata1,nread,stdid,cid,plevel,&
                  yr,mon,day,hr,shr,rmiss_4)
   call creat_ctl_stas(dtype,nk,ndata1,ndata1,stdid,cid,plevel,rmiss_4)
   call creat_ctl_station(dtype,nk,plevel,rmiss_4)

!! calculate statis  distribution at each level

    call stas_hist_spdir(dtype,stdid,cid,rmean,std,nom_std,plevel,ndata1,ndata1,nk,rmiss_4)
    call stas_wscal(dtype,stdid,cid,nk,ndata1,ndata1,nread,tdata)

!!   To filter stations without records for the past one month
!!   To filter stations with most records rejected by GSI gross check

      print *,'stas: starting to filter bad data'

   outfile=trim(dtype)//'_discnt_list'
   outfile1=trim(dtype)//'_qlyrej_list'

      open(70,file=outfile,form='formatted')

    n_tdata=0
   istart=nread-n_miss_crit-1
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
        if(n_tdata(i,k) == n_miss_crit) then
          kcount=kcount+1
        endif
      enddo
       if (kcount == 43 ) then
         write(70,100) stdid(i),i,cid(i,1),cid(i,2)
       endif
    enddo

100 format(2x,a8,i8,2f8.2)


!!! call quality control procedure

!     call w_select(dtype,stdid,cid,std,std_no,tdata_no2,n_tdata,&
!                   rmean,break,xmed,xb,sb,smad,plevel,nk,2000,ndata1,rmiss)


   file1=trim(dtype)//'_station' 

    open(10,file=file1,form='unformatted')
     write(10) ndata1,nread,n_miss_crit 
     write(10) stdid(1:ndata1)
     write(10) cid(1:ndata1,:) 
     write(10)  n_tdata(1:ndata1,1:nk)
     write(10)  tdata_no2(1:ndata1,1:nk,1:3)
     write(10) tdata_no3(1:ndata1,1:nk,1:nread)
  

close(10)

   file1=trim(dtype)//'_stas' 

 open (20,file=file1,form='unformatted')
  allocate(tmp(ndata1,6))
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
   tmp(1:ndata1,5:6)=xb(1:ndata1,k,:)
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

   file1=trim(dtype)//'_stas_station' 

 open (25,file=file1,form='unformatted')

  allocate(tmp2(18))

 do i=1,ndata1
    stid=stdid(i)
    rlat=cid(i,2)
    rlon=cid(i,1)
   do k=1,nk
    write(25) stid,rlat,rlon,rtim,1,0
      if(k == nk .and.std_no(i,k,1) >0.0 ) print *,plevel(k),rmean(i,k,1),rmean(i,k,2),std(i,k,1),std(i,k,2),std_no(i,k,1)
      tmp2(1)=rmean(i,k,1)
      tmp2(2)=rmean(i,k,2)
      tmp2(3)=rmean(i,k,3)
      tmp2(4)=rmean(i,k,4)
      tmp2(5)=rmean(i,k,5)
      tmp2(6)=rmean(i,k,6)
      tmp2(7)=std(i,k,1)
      tmp2(8)=std(i,k,2)
      tmp2(9)=std(i,k,3)
      tmp2(10)=std(i,k,4)
      tmp2(11)=std(i,k,5)     ! direction
      tmp2(12)=std(i,k,6)     ! speed 
      tmp2(13)=xmed(i,k,1)
      tmp2(14)=xmed(i,k,2)
      tmp2(15)=smad(i,k,1)
      tmp2(16)=smad(i,k,2)
      tmp2(17)=std_no(i,k,1)
      tmp2(18)=tdata_no2(i,k,2)
      write(25) plevel(k),tmp2
    enddo
  enddo

!  the end of file
     stid='        '
     write(25) stid,xlat0,xlon0,rtim,nlev0,nflag0

      deallocate(tmp2)
close(25)

            
!   write time series files

   file1=trim(dtype)//'_time' 

  open (30,file=file1,form='unformatted')         

  allocate (tmp2(ndata1))

  do it=1,nread

   do k=1,nk
    tmp2(1:ndata1)=tdata(1:ndata1,k,9,it)
    write(30) tmp2
   enddo
   do k=1,nk
    tmp2(1:ndata1)=tdata(1:ndata1,k,10,it)
    write(30) tmp2
   enddo
   do k=1,nk
    tmp2(1:ndata1)=tdata(1:ndata1,k,11,it)
    write(30) tmp2
   enddo
   do k=1,nk
    tmp2(1:ndata1)=tdata(1:ndata1,k,12,it)
    write(30) tmp2
   enddo
   do k=1,nk
    tmp2(1:ndata1)=tdata(1:ndata1,k,13,it)
    write(30) tmp2
   enddo
   do k=1,nk
    tmp2(1:ndata1)=tdata(1:ndata1,k,14,it)
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
    tmp2(1:ndata1)=tdata_no(1:ndata1,k,1,it)
    write(30) tmp2
   enddo
   do k=1,nk
    tmp2(1:ndata1)=tdata_no(1:ndata1,k,2,it)
    write(30) tmp2
   enddo
      
enddo

   close(30)

   file1=trim(dtype)//'_station.list' 

    open(50,file=file1,form='formatted')

    do i=1,ndata1
       if(cid(i,5) >100000.00) cid(i,5)=-99.0
       icid=nint(cid(i,5))
       write(50,1000) stdid(i),cid(i,1),cid(i,2),icid
    enddo

1000 format(a8, 2f10.2,i10)

      file1=trim(dtype)//'_lowsp_stationlist'

       open(60,file=file1,form='unformatted')

       do k=1,nk
        write(60) lspno(1:ndata1,k)
       enddo

       close(60)

        do i=1,51
         hist(i)=(i-1)*1.0
        enddo

     lsp_hist=0.0

     do k=1,nk
     do i=1,ndata1
        jj=0
       do j=2,51
         if(lspno(i,k) == 0) then
           jj=1
           exit
         else if(lspno(i,k) >0 .and. lspno(i,k) <=hist(j)) then
           jj=j
           exit
        else if(lspno(i,k) >0 .and. lspno(i,k) >hist(51)) then
           jj=51
          exit
        endif
       enddo
       if (jj >0) lsp_hist(jj,k)=lsp_hist(jj,k)+1.0
  enddo
  enddo

      
      all_lsp=0.0
      do k=1,nk
      do i=1,ndata1
        all_lsp(k)=all_lsp(k)+lspno(i,k)
      enddo
      enddo
      


      file1=trim(dtype)//'_lowsp_hist'
       open(70,file=file1,form='unformatted')
         do k=1,nk
           write(70) lsp_hist(1:51,k)
         enddo
         
   close(70) 


        file1=trim(dtype)//'_lspdis'
          open(80,file=file1,form='formatted')
          write(80,250) (all_lsp(k),k=1,nk) 
250  format(10f7.1)

      close(80)

      deallocate(stdid,cid,cid_time,tdata,tdata_no,tdata_no3,tdata_no2,&
               std,std_no,rmean,break,xmed,xb,sb,smad,n_tdata,n_stas,nom_std,lspno)
       
   return 
   end
    
   
  
