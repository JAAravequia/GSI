!   read statistics files from different cycle delay (tm00,tm03 ..)
!   compare their values  


      subroutine compare_sfc(dtype) 

       character(5) dtype
       character(1) ddtype
       character(len=8),allocatable,dimension(:) :: stdid1,stdid2,stdid_00,stdid_03,stdid_06,stdid_09,stdid_12
       real(4),allocatable,dimension(:,:) :: cid1,cid2,cid_00,cid_03,cid_06,cid_09,cid_12
       real(4),allocatable,dimension(:,:) :: std_00,std_03,std_06,std_09,std_12
       real(4),allocatable,dimension(:) :: std_no_00,std_no_03,std_no_06,std_no_09,std_no_12
       real(4),allocatable,dimension(:,:) :: rmean_00,rmean_03,rmean_06,rmean_09,rmean_12,rdiff1,rdiff2
       real(4),allocatable,dimension(:,:) :: tdata_no2_00,tdata_no2_03,tdata_no2_06,tdata_no2_09,tdata_no2_12


        character(100) file1,file2
 
     ddtype=dtype(1:1)
     ddtype=trim(ddtype)

     print *, ddtype

!  for tm00
     file1=trim(dtype)//'_station_00'
     file2=trim(dtype)//'_stas_00'
     open(11,file=file1,form='unformatted')
     rewind(11)
     read(11) ndata,nread,n_miss_crit
      allocate(stdid_00(ndata),cid_00(ndata,6),tdata_no2_00(ndata,3),std_no_00(ndata),stdid1,cid1(ndata,2))
     if(trim(ddtype) == 'u') then
        allocate(std_00(ndata,6),rmean_00(ndata,6))
        n=6
     else
        allocate(std_00(ndata,5),rmean_00(ndata,5))
        n=5
     endif
     close(11)
     call read_list_sfc(dtype,file1,file2,n,ndata,stdid_00,cid_00,tdata_no2_00,std_00,rmean_00,std_no_00)

!   for tm03

     file1=trim(dtype)//'_station_03'
     file2=trim(dtype)//'_stas_03'
     open(21,file=file1,form='unformatted')
     rewind(21)
     read(21) ndata1,nread,n_miss_crit
      allocate(stdid_03(ndata1),cid_03(ndata1,6),tdata_no2_03(ndata1,3),&
               std_no_03(ndata1),stdid2(ndata1),rdiff2(ndata1))
     if(trim(ddtype) == 'u') then
        allocate(std_03(ndata1,6),rmean_03(ndata1,6))
        n=6
     else
        allocate(std_03(ndata1,5),rmean_03(ndata1,5))
        n=5
     endif
     close(21)
     call read_list_sfc(dtype,file1,file2,n,ndata1,stdid_03,cid_03,tdata_no2_03,std_03,rmean_03,std_no_03)

! for tm06

     file1=trim(dtype)//'_station_06'
     file2=trim(dtype)//'_stas_06'
     open(31,file=file1,form='unformatted')
     rewind(31)
     read(31) ndata2,nread,n_miss_crit
      allocate(stdid_06(ndata2),cid_06(ndata2,6),tdata_no2_06(ndata2,3),std_no_06(ndata2))
     if(trim(ddtype) == 'u') then
        allocate(std_06(ndata2,6),rmean_06(ndata2,6))
        n=6
     else
        allocate(std_06(ndata2,5),rmean_06(ndata2,5))
        n=5
     endif
     close(31)
     call read_list_sfc(dtype,file1,file2,n,ndata2,stdid_06,cid_06,tdata_no2_06,std_06,rmean_06,std_no_06)

! for tm09

     file1=trim(dtype)//'_station_09'
     file2=trim(dtype)//'_stas_09'
     open(41,file=file1,form='unformatted')
     rewind(41)
     read(41) ndata3,nread,n_miss_crit
      allocate(stdid_09(ndata3),cid_09(ndata3,6),tdata_no2_09(ndata3,3),std_no_09(ndata3))
     if(trim(ddtype) == 'u') then
        allocate(std_09(ndata3,6),rmean_09(ndata3,6))
        n=6
     else
        allocate(std_09(ndata3,5),rmean_09(ndata3,5))
        n=5
     endif
     close(41)
     call read_list_sfc(dtype,file1,file2,n,ndata3,stdid_09,cid_09,tdata_no2_09,std_09,rmean_09,std_no_09)

! for tm12

     file1=trim(dtype)//'_station_12'
     file2=trim(dtype)//'_stas_12'
     open(51,file=file1,form='unformatted')
     rewind(51)
     read(51) ndata4,nread,n_miss_crit
      allocate(stdid_12(ndata4),cid_12(ndata4,6),tdata_no2_12(ndata4,3),std_no_12(ndata4))
     if(trim(ddtype) == 'u') then
        allocate(std_12(ndata4,6),rmean_12(ndata4,6))
        n=6
     else
        allocate(std_12(ndata4,5),rmean_12(ndata4,5))
        n=5
     endif
     close(51)
     call read_list_sfc(dtype,file1,file2,n,ndata4,stdid_12,cid_12,tdata_no2_12,std_12,rmean_12,std_no_12)


      write(6,*)  ' the comparison between 00,06,12'
      write(6,*) ndata,ndata2,ndata4
      n_count=0
      do i=1,ndata
      do j=1,ndata2
      do k=1,ndata4
         if(trim(stdid_00(i)) == trim(stdid_06(j)) .and. trim(stdid_00(i)) == &
            trim(stdid_12(k)))  then
            ncount=ncount +1
            write(6,100) stdid_00(i),cid_00(i,1),cid_00(i,2),rmean_00(i,1),&
                         rmean_06(j,1),rmean_12(k,1),rmean_00(i,2),&
                         rmean_06(j,2),rmean_12(k,2)
            write(6,110) std_00(i,1),std_06(j,1),std_12(k,1),std_00(i,2),&
                         std_06(j,2),std_12(k,2),std_no_00(i),std_no_06(j),&
                         std_no_12(k)
            write(6,110) tdata_no2_00(i,1),tdata_no2_06(j,1),tdata_no2_12(k,1),&
                         tdata_no2_00(i,2),tdata_no2_06(j,2),tdata_no2_12(k,2),&
                         tdata_no2_00(i,3),tdata_no2_06(j,3),tdata_no2_12(k,3)  
                 stdid1(ncount)=stdid_00(i)
                 cid1(ncount,1)=cid_00(i,1)
                 cid1(ncount,2)=cid_00(i,2)
                 rdiff1(ncount,1)=rmean_00(i,1)
                 rdiffr1(ncount,2)=rmean_00(j,1)
                 rdiff1(ncount,3)=rmean_00(k,1)
                 rdiff1(ncount,4)=std_00(i,1)
                 rdiff1(ncount,5)=std_00(j,1)
                 rdiff1(ncount,6)=std_00(k,1)
            exit
         endif
      enddo
      enddo
      enddo

       write(6,*) ' total same station number,ncount=',ncount

100  format(a8,8f8.2)
110  format(9f8.2)

      write(6,*)  ' the comparison between 00,09'
      write(6,*) ndata1,ndata3
       ncount2=0
      do i=1,ndata1
      do j=1,ndata3
         if(trim(stdid_03(i)) == trim(stdid_09(j))) then 
            ncount2=ncount2 +1
            write(6,120) stdid_03(i),cid_03(i,1),cid_03(i,2),rmean_03(i,1),&
                         rmean_09(j,1),rmean_03(i,2),rmean_09(j,2)
            write(6,130) std_03(i,1),std_09(j,1),std_03(i,2),std_09(j,2),&
                         std_no_03(i),std_no_09(j)
            write(6,130) tdata_no2_03(i,1),tdata_no2_09(j,1),tdata_no2_03(i,2),&
                         tdata_no2_09(j,2),tdata_no2_03(i,3),tdata_no2_09(j,3)
            stdid2(ncount2)=stdid_00(i)
            cid2(ncount2,1)=cid_00(i,1)
            cid2(ncount2,2)=cid_00(i,2)
            rdiff2(ncount2,1)=rmean_00(i,1)
            rdiffr2(ncount2,2)=rmean_00(j,1)
            rdiff2(ncount2,3)=rmean_00(k,1)
            rdiff2(ncount2,4)=std_00(i,1)
            rdiff2(ncount2,5)=std_00(j,1)
            rdiff2(ncount2,6)=std_00(k,1)
            exit
         endif
      enddo
      enddo

     write(6,*) ' total same station number,ncount2=',ncount2 
     
       
     
120  format(a8,6f8.2)
130  format(6f8.2)

      deallocate(stdid_00,stdid_06,stdid_09,stdid_12,stdid_03)
      deallocate(std_00,std_06,std_09,std_12,std_03)
      deallocate(rmean_00,rmean_06,rmean_09,rmean_12,rmean_03)
      deallocate(std_no_00,std_no_06,std_no_09,std_no_12,std_no_03)
      deallocate(tdata_no2_00,tdata_no2_06,tdata_no2_09,tdata_no2_12,tdata_no2_03)


     return
     end
