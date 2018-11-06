!  the program set criteria for potential rejection list of stations and bias list
!  of the surface pressure stations


      subroutine make_list_time_sfc(dtype,nd) 
      integer,allocatable,dimension(:) :: n_tdata
      character(len=8),allocatable,dimension(:) :: stdid
      real(4),allocatable,dimension(:,:) :: cid
      real(4),allocatable,dimension(:,:) :: tdata_no3,tdata_no2

      
     character(5) dtype,tm
     character(1) ddtype
     character(8)  stid
     character(len=8),dimension(7000) :: stdid_bias,stdid_rej
     character(100) file,file2,file3,file4,file5
     logical bias,rej,qrej,oth



      file=trim(dtype)//'_station'
      file2=trim(dtype)//'_bias_list'
      file3=trim(dtype)//'_rej_list'

       bias= .false.
       rej= .false.
       qrej= .false.
       oth= .false.
       
     open(11,file=file,form='unformatted')
     rewind(11)
     read(11) ndata,nread,n_miss_crit 

     print *,'ndata=',ndata,nread,n_miss_crit

     allocate(tdata_no3(ndata,nread),tdata_no2(ndata,3),n_tdata(ndata),stdid(ndata),cid(ndata,6))

     read(11) stdid(1:ndata),cid(1:ndata,:)
     read(11) n_tdata(1:ndata)
     read(11) tdata_no2(1:ndata,:)
     read(11) tdata_no3(1:ndata,1:nread)

deallocate(tdata_no3,tdata_no2,n_tdata)

          close(11)

        call read_list2_sfcformake(dtype,file2,nd,7000,ncount_bias,stdid_bias)
        call read_list2_sfcformake(dtype,file3,nd,7000,ncount_rej,stdid_rej)

        file=trim(dtype)//'_stationlist'

        open(21,file=file,form='formatted')

         do i=1,ndata
            bias= .false.
            rej= .false.
            stid=stdid(i)
            ddtype=stid(8:8)
            if(trim(ddtype) == 'a' .or. trim(ddtype) == 'x') then
               stid=stid(1:7)
               stdid(i)=trim(stid)
            endif
            print *, 'stdid=',stdid(i),i
            if(ncount_bias >0) then
               do j=1,ncount_bias
                  stid=stdid_bias(j)
                  ddtype=stid(8:8)
                  if(trim(ddtype) == 'a' .or. trim(ddtype) == 'x') then
                     stdid_bias(j)=stid(1:7)
                  endif
                  if(trim(stdid(i)) == trim(stdid_bias(j)) ) then
                     write(21,100) stdid(i),cid(i,2),cid(i,1)
                     bias=.true.
                     exit
                  endif
               enddo
!               print *,'bias=',bias ,i
            endif
            if(ncount_rej >0 .and. .not. bias)  then
               print *,'ncount_rej=',ncount_rej
               do j=1,ncount_rej
                  stid=stdid_rej(j) 
                  ddtype=stid(8:8)
                  if(trim(ddtype) == 'a' .or. trim(ddtype) == 'x') then
                     stdid_rej(j)=stid(1:7)
                  endif
                  if(trim(stdid(i)) == trim(stdid_rej(j)) ) then
                     write(21,150) stdid(i),cid(i,2),cid(i,1)
                     rej=.true.
                     exit
                  endif
               enddo
!               print *,'rej=',rej,i
            endif
            if(  .not. bias .and. .not. rej .and. .not. qrej .and. &
                    .not. oth ) then
               write(21,300) stdid(i),cid(i,2),cid(i,1)
               write(6,300) stdid(i),cid(i,2),cid(i,1)
            endif
         enddo
            
100    format(a8,';',f8.2,';',f8.2,';  bias')
150    format(a8,';',f8.2,';',f8.2,';  reje')
300    format(a8,';',f8.2,';',f8.2,';  norm')
          
        close(21)

  deallocate(stdid,cid)
        return
        end
