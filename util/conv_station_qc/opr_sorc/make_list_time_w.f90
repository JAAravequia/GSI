!  the program set criteria for potential rejection list of stations and bias list
!  of the surface pressure stations


      subroutine make_list_time_w(dtype,tm,nk)
      integer,allocatable,dimension(:,:) :: n_tdata
      character(len=8),allocatable,dimension(:) :: stdid
      real(4),allocatable,dimension(:,:) :: cid
      real(4),allocatable,dimension(:,:,:) :: tdata_no3,tdata_no2

      
     character(5) dtype,tm
     character(100) file

     
      file=trim(dtype)//'_station_'//trim(tm)

     print *, dtype,nk

     open(11,file=file,form='unformatted')
     rewind(11)
     read(11) ndata,nread,n_miss_crit 

     allocate(tdata_no3(ndata,nk,nread),tdata_no2(ndata,nk,3),n_tdata(ndata,nk),stdid(ndata),cid(ndata,6))

     read(11) stdid(1:ndata),cid(1:ndata,:)
     read(11) n_tdata(1:ndata,1:nk)
     read(11) tdata_no2(1:ndata,1:nk,:)
     read(11) tdata_no3(1:ndata,1:nk,1:nread)


          close(11)

        file=trim(dtype)//'_stationlist_'//trim(tm)

        open(21,file=file,form='formatted')

         do i=1,ndata
            write(21,100) stdid(i),cid(i,2),cid(i,1)
         enddo
100    format(a8,';',f8.2,';',f8.2)

        close(21)
        return 
        end
