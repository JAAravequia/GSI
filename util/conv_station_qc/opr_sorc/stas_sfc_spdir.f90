!   the program read the data 

!   array for data tdata items: 1: station data number; 2: vertical level
!   3: data items, 4: time 
!   data items: 1: uo-ub;2:uo-ua;3:variational qc weight (bac);4: variational
!   qc weight (ana);5:vo-vb;6:vo-va;7: uobs;8:vobs;9:directionO-directionB;  
!   10: directionO-directionA;11: speedO-speedB;12:speedO-speedB;13:direction
!   obs;14: speed obs

     subroutine stas_sfc_spdir(dtype,yr,mon,day,hr,shr,iqcflg)

    real(4),allocatable,dimension(:) :: tmp2
    real(4),allocatable,dimension(:,:) :: tmp,tmp22,tmp24,tmp26,tmp28,tmp20,tmp30

    character(len=3) :: mon
    character(len=4) :: shr,yr
    character(len=2) :: day,hr

    integer,dimension(4) :: iqcflg
    

    real(4) all_lsp

    real(4),dimension(51) :: hist
    real(4),dimension(1) :: plevel
    real(4),dimension(51)   :: lsp_hist
    character(5) dtype

    character(len=8),allocatable,dimension(:) :: stdid,stdid_tmp
    real(4),allocatable,dimension(:,:,:) :: tdata
    real(8),allocatable,dimension(:,:) :: std,std_no,rmean,break,xmed,xb,sb,smad
    real(8),allocatable,dimension(:,:) :: stdobs,rmeanobs
    real(8),allocatable,dimension(:) :: nom_std,stdobs_no 
    real(4),allocatable,dimension(:) :: lspno 
    real(4),allocatable,dimension(:,:,:) :: tdata_no,cid_time,tdata_tmp
    real(4),allocatable,dimension(:,:) :: cid,tdata_no3,tdata_no2
    integer,allocatable,dimension(:,:) :: n_stas
    integer,allocatable,dimension(:) :: n_tdata,itmp
    

    real(8),dimension(450) :: xtmp,xtmp2
!    integer,dimension(35000,2) :: n_stas
    real(8) xb1,sb1,smad1,xmed1,xmed2,xmad1,xmad2
    
    character(8)  stid 
    real(4) rlat,rlon,xlat0,xlon0,rtim,nlev0,nflag0

    character(100) cfile
    character(50) fname,infile,file1,outfile,outfile1

    real(8) rmiss
    real(4) rmiss_4
    integer i_break

! data dtype /'w220'/
 data rmiss/-999.0/ 
 data rmiss_4/-999.0/ 
 data lunin/51/ 
data plevel/1000.0/

  rtim=0.0
  nflg0=0
  xlat0=0.0
  xlon0=0.0
  nlev0=0
  rm_speed=0.0
  rn_speed=0.0
  std_speed=0.0
  i_break=28

  infile=trim(dtype)//'_out'

   open(lunin,file=infile,form='unformatted')

   rewind(lunin)

   read(lunin) ndata1,nread,n

   if( n /=  14 ) then
     print *, 'wrong number for n, should be 14 ',n
     stop
    end if

     print *, 'stas_sfc_spdir: ndata1=',ndata1,nread,n
   allocate(stdid(ndata1),cid(ndata1,6),cid_time(ndata1,6,nread),tdata(ndata1,14,nread))
   allocate(tdata_no(ndata1,2,nread),tdata_no2(ndata1,3),tdata_no3(ndata1,nread))
   allocate(n_tdata(ndata1),n_stas(ndata1,2),nom_std(ndata1))
   allocate(std(ndata1,6),std_no(ndata1,6),rmean(ndata1,6),break(ndata1,6))
   allocate(stdobs(ndata1,2),stdobs_no(ndata1),rmeanobs(ndata1,2))
   allocate(xmed(ndata1,2),xb(ndata1,2),sb(ndata1,2),smad(ndata1,2),lspno(ndata1))

    tdata_no=0.0
    tdata=0.0
    tdata_no2=0.0
    tdata_no3=0.0
  stid='        '

   read(lunin) stdid(1:ndata1),cid(1:ndata1,:)
    read(lunin) cid_time(1:ndata1,:,1:nread)
    read(lunin) tdata(1:ndata1,:,1:nread)
    read(lunin) tdata_no(1:ndata1,:,1:nread)

   
    write(6,500) tdata(5,1,5)
500 format(f8.2)

    n_miss_crit=0
      n_miss_crit=nread/3

   
   lspno=0.0
   do i=1,ndata1
   do iread=1,nread
       if(tdata(i,14,iread) >-800.0 .and. tdata(i,14,iread) <=0.3) then
           lspno(i)=lspno(i)+1.0
       endif
   enddo
   enddo

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
   enddo

   std=0.0
   rmean=0.0
   std_no=0.0
   stdobs=0.0
   rmeanobs=0.0
   stdobs_no=0.0
   do i=1,ndata1
   do j=1,6
   do it=1,nread
    jj=j+8
    if(tdata(i,jj,it) >-300.0) then
      std_no(i,j)=std_no(i,j)+1.0
      rmean(i,j)=rmean(i,j)+tdata(i,jj,it)
    endif
  enddo
  enddo
  enddo

  do i=1,ndata1
  do j=1,6
   if(std_no(i,j) >=1.0) then
     rmean(i,j)=rmean(i,j)/std_no(i,j)
   else
     rmean(i,j)=rmiss
   endif
  enddo
  enddo
 
  do i=1,ndata1
   do j=1,6
   do it=1,nread
    jj=j+8
    if(tdata(i,jj,it) >-300.0 .and. rmean(i,j) >-300.0) then
      std(i,j)=std(i,j)+(tdata(i,jj,it)-rmean(i,j))*(tdata(i,jj,it)-rmean(i,j))
    endif
  enddo
  enddo
  enddo



  do i=1,ndata1
  do j=1,6
   if(std_no(i,j) >1.0) then
     std(i,j)=sqrt(std(i,j)/std_no(i,j))
   else if(std_no(i,j) ==1.0) then
     std(i,j)=0.0
   else
     std(i,j)=rmiss
   endif
  enddo
  enddo

! calculate normalized o-b speed standard deviation 

  nom_std=0.0
  do i=1,ndata1
     if(rmean(i,6) >1.0)  then 
        nom_std(i)=std(i,3)/rmean(i,6)    
     else
        nom_std(i)=0.0
     endif
  enddo


! calculate the median and other robster statistics

 n_stas=0
   xmed=rmiss
   smad=rmiss
   break=rmiss

    do i=1,ndata1
    do it=1,nread
      if(abs(tdata(i,9,it)) < 300.0) then
        n_stas(i,1)=n_stas(i,1)+1
        xtmp(n_stas(i,1))=tdata(i,9,it)
      endif
      if(abs(tdata(i,11,it))< 900.0) then
        n_stas(i,2)=n_stas(i,2)+1
        xtmp2(n_stas(i,2))=tdata(i,11,it)
      endif
    enddo
     nn1=n_stas(i,1)
     nn2=n_stas(i,2)
    if(nn1 >=5) then
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


!  calculate past 5 days observation standard deviation to filer constant
!  observation in the past 5 days

  istart=nread-20                   ! for katest 5 days observation, if constant, rejected
  do i=1,ndata1
  do j=13,14
   jj=j-12
  do it=istart,nread
    if(tdata(i,j,it) >-300.0) then
       rmeanobs(i,jj)=rmeanobs(i,jj)+tdata(i,j,it)
       stdobs_no(i)=stdobs_no(i)+1.0
    endif
  enddo
  enddo
  enddo

  do i=1,ndata1
  do j=1,2
     if(stdobs_no(i) >1.0) then
        rmeanobs(i,j)=rmeanobs(i,j)/stdobs_no(i)
     else
        rmeanobs(i,j)=rmiss
     endif
  enddo
  enddo
    
  do i=1,ndata1
  do it=istart,nread
  do j=13,14
   jj=j-12
    if(tdata(i,j,it) >-300.0 .and. rmeanobs(i,jj) >-399.0) then
      stdobs(i,jj)=stdobs(i,jj)+(tdata(i,j,it)-rmeanobs(i,jj))*(tdata(i,j,it)-rmeanobs(i,jj))
    endif
  enddo
    if(trim(stdid(i)) =='16008') then
      print * ,'stas_sfc:',tdata(i,13,it),tdata(i,14,it),stdobs(i,1),stdobs(i,2),rmeanobs(i,1),rmeanobs(i,2)
    endif
  enddo
  enddo



  do i=1,ndata1
  do j=1,2
   if(stdobs_no(i) >1.0 ) then
      stdobs(i,j)=sqrt(stdobs(i,j)/stdobs_no(i))
   else if (stdobs_no(i)  <1.0) then
     stdobs(i,j)=rmiss
   else if (stdobs_no(i) ==1.0) then
     stdobs(i,j)=0.0
   endif
  enddo
  enddo





!!   To filter stations without records for the past one month
!!   To filter stations with most records rejected by GSI gross check

      print *,'stas_sfc_spdir: starting to filter bad data'

      outfile=trim(dtype)//'_discnt_list'

      open(70,file=outfile,form='formatted')

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
         write(70,100) stdid(i),i,cid(i,1),cid(i,2),std_no(i,1),rmean(i,1),rmean(i,3),&
                       std(i,1),std(i,3),tdata_no2(i,1),tdata_no2(i,2),iqcflg(1)
         write(70,110) xmed(i,1),xmed(i,2),smad(i,1),smad(i,2)
         write(70,120) n_tdata(i),break(i,1), break(i,2),break(i,3), break(i,4)
       endif
    enddo

100 format(2x,a8,i8,9f8.2,i3)
110 format(16x,8f8.2)
120 format(16x,i8,4f8.2)
130 format(16x,4f8.2)

      close(70)

!for the station with almost constant observations
      outfile=trim(dtype)//'consobs_list'
      open(71,file=outfile,form='formatted')
      n_const=0

       do i=1, ndata1
          if(stdobs(i,1) <=0.05 .and. stdobs_no(i) >=5.0 .and.  rmeanobs(i,1) >-900.0 ) then 
             n_const=n_const+1
             cid(i,6)=-100.0
             write(71,100) stdid(i),i,cid(i,1),cid(i,2),std_no(i,1),rmean(i,1),rmean(i,3),&
                       std(i,1),std(i,3),tdata_no2(i,1),tdata_no2(i,2),iqcflg(2)
             write(71,110) xmed(i,1),xmed(i,2),xb(i,1),xb(i,2),sb(i,1),sb(i,2),smad(i,1),smad(i,2)
             write(71,120) n_tdata(i),break(i,1), break(i,2),break(i,3), break(i,4)
             write(71,130) rmeanobs(i,1),rmeanobs(i,2),stdobs(i,1),stdobs(i,2) 
          else if(stdobs(i,2) <=0.05 .and. stdobs_no(i) >=5.0 .and.  rmeanobs(i,2) >-900.0 ) then 
             n_const=n_const+1
             cid(i,6)=-100.0
             write(71,100) stdid(i),i,cid(i,1),cid(i,2),std_no(i,1),rmean(i,1),rmean(i,3),&
                       std(i,1),std(i,3),tdata_no2(i,1),tdata_no2(i,2),iqcflg(2)
             write(71,110) xmed(i,1),xmed(i,2),xb(i,1),xb(i,2),sb(i,1),sb(i,2),smad(i,1),smad(i,2)
             write(71,120) n_tdata(i),break(i,1), break(i,2),break(i,3), break(i,4)
             write(71,130) rmeanobs(i,1),rmeanobs(i,2),stdobs(i,1),stdobs(i,2) 
          endif
       enddo

         print *,'n_const=',n_const


      close(71)

     
  call stas_wscal_sfc(dtype,stdid,cid,ndata1,ndata1,nread,tdata)


 !    call w_select_sfc(dtype,stdid,cid,std,std_no,tdata_no2,n_tdata,&
 !                      rmean,break,xmed,xb,sb,smad,35000,ndata1,rmiss_4)


     print *,'stas_sfc_spdir: finish to filter bad data'
   file1=trim(dtype)//'_station' 
    open(10,file=file1,form='unformatted')

     nstation=0
      allocate(tmp(ndata1,6),tmp22(ndata1,3),tmp24(ndata1,nread),stdid_tmp(ndata1),itmp(ndata1))
      do i=1,ndata1
        if(rmean(i,1) >-900.0) then
          if(cid(i,6) <0.0) print *,'stas_sfc_spdir:stdid=',stdid(i),cid(i,6)
          nstation=nstation+1
          stdid_tmp(nstation)=stdid(i)
          tmp(nstation,1:6)=cid(i,1:6)
          itmp(nstation)=n_tdata(i)
          tmp22(nstation,1:3)=tdata_no2(i,1:3)
          tmp24(nstation,1:nread)=tdata_no3(i,1:nread)
        endif
      enddo

     print *,'nstation,nread,n_miss_crit=',nstation,nread,n_miss_crit

     write(10) nstation,nread,n_miss_crit
     write(10) stdid_tmp(1:nstation),tmp(1:nstation,:)
     write(10) itmp(1:nstation)
     write(10) tmp22(1:nstation,:)
     write(10) tmp24(1:nstation,1:nread)


close(10)

    deallocate(tmp24)

    
   file1=trim(dtype)//'_stas' 
 open (20,file=file1,form='unformatted')

    allocate(tmp26(nstation,6),tmp20(nstation,6),tmp24(nstation,6),tmp28(nstation,6),tmp30(nstation,6))

   nstation2=0
   do i=1,ndata1
    if(rmean(i,1) >-900.0) then
      nstation2=nstation2+1
      tmp26(nstation2,1:6)=rmean(i,1:6)
      tmp20(nstation2,1:6)=std(i,1:6)
      tmp24(nstation2,1:2)=xmed(i,1:2)
      tmp24(nstation2,3:4)=smad(i,1:2)
      tmp24(nstation2,5:6)=xb(i,1:2)
      tmp28(nstation2,1:6)=break(i,1:6)
      tmp30(nstation2,1:6)=std_no(i,1:6)
    endif
  enddo

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


  call stas_hist_spdir_sfc(dtype,stdid_tmp,tmp,tmp26,tmp20,nom_std,ndata1,nstation,rmiss_4)
  call creat_ctl_stas(dtype,1,ndata1,nstation,stdid_tmp,tmp,plevel,rmiss_4)
  call creat_ctl_station(dtype,1,plevel,rmiss_4)
  call creat_ctl_time_sfc(dtype,ndata1,nstation,6,nread,stdid_tmp,tmp,tmp20,tmp26,tmp24,&
       yr,mon,day,hr,shr,rmiss_4)

   file1=trim(dtype)//'_stas_station' 

 open (25,file=file1,form='unformatted')

  allocate(tmp2(18))

 do i=1,nstation
    stid=stdid_tmp(i)
    rlat=tmp(i,2)
    rlon=tmp(i,1)
    write(25) stid,rlat,rlon,rtim,1,1
    tmp2(1)=tmp26(i,1)
      tmp2(2)=tmp26(i,2)
      tmp2(3)=tmp26(i,3)
      tmp2(4)=tmp26(i,4)
      tmp2(5)=tmp26(i,5)
      tmp2(6)=tmp26(i,6)
      tmp2(7)=tmp20(i,1)
      tmp2(8)=tmp20(i,2)
      tmp2(9)=tmp20(i,3)
      tmp2(10)=tmp20(i,4)
      tmp2(11)=tmp20(i,5)     ! direction
      tmp2(12)=tmp20(i,6)     ! speed
      tmp2(13)=tmp24(i,1)
      tmp2(14)=tmp24(i,2)
      tmp2(15)=tmp24(i,3)
      tmp2(16)=tmp24(i,4)
      tmp2(17)=tmp30(i,1)
      tmp2(18)=tmp22(i,2)
      write(25) tmp2
  enddo

!  the end of file
     stid='        '
     write(25) stid,xlat0,xlon0,rtim,nlev0,nflag0

close(25)


            
!   write time series files

   file1=trim(dtype)//'_time' 

   print *, 'stas_sfc_spdir:',dtype,file1
   allocate(tdata_tmp(ndata1,10,nread))

  open (30,file=file1,form='unformatted')         

    nstation3=0
   do i=1,ndata1
     if(rmean(i,1) >-900.0) then
        nstation3=nstation3+1
        tdata_tmp(nstation3,1:6,1:nread)=tdata(i,9:14,1:nread)
        tdata_tmp(nstation3,7:8,1:nread)=tdata_no(i,3:4,1:nread)
        tdata_tmp(nstation3,9:10,1:nread)=tdata_no(i,1:2,1:nread)
     endif
   enddo

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
    tmp2(1:nstation)=tdata_tmp(1:nstation,8,it)
    write(30) tmp2
    tmp2(1:nstation)=tdata_tmp(1:nstation,9,it)
    write(30) tmp2
    tmp2(1:nstation)=tdata_tmp(1:nstation,10,it)
    write(30) tmp2
enddo

   close(30)


1000 format(a8, 2f10.2,i10)

      file1=trim(dtype)//'_lowsp_stationlist'

       open(60,file=file1,form='unformatted')

        write(60) lspno(1:ndata1)

       close(60)

        do i=1,51
         hist(i)=(i-1)*1.0
        enddo

     lsp_hist=0.0

     do i=1,ndata1
        jj=0
       do j=1,50
         if(lspno(i) >0 .and. lspno(i) >hist(51)) then
           jj=50
           exit
          else if(lspno(i) == 0) then
           jj=1
           exit
         else if(lspno(i) >=hist(j) .and. lspno(i) <=hist(j+1)) then
           jj=j
           exit
        endif
     enddo
       if (jj >0) lsp_hist(jj)=lsp_hist(jj)+1.0
  enddo

      
      all_lsp=0.0
      do i=1,ndata1
        all_lsp=all_lsp+lspno(i)
      enddo
      


      file1=trim(dtype)//'_lowsp_hist'
       open(70,file=file1,form='unformatted')
           write(70) lsp_hist(1:51)
         
   close(70) 


        file1=trim(dtype)//'_lspdis'
          open(80,file=file1,form='formatted')
          write(80,250) all_lsp 
250  format(f7.1)

      close(80)

      deallocate(nom_std,lspno)
      deallocate(stdid,stdid_tmp,tdata,std,std_no,rmean,break,xmed,xb,sb,smad)
      deallocate(stdobs,rmeanobs,stdobs_no,tdata_no,cid_time,tdata_tmp)
      deallocate(cid,tdata_no3,tdata_no2,n_stas,n_tdata,itmp)
    deallocate(tmp,tmp2,tmp20,tmp22,tmp24,tmp26,tmp30,tmp28)
       
   return 
   end
    
   
  
