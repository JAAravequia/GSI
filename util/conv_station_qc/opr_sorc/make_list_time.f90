!  the program set criteria for potential rejection list of stations and bias list
!  of the surface pressure stations


      subroutine make_list_time(dtype,nd,nk)
      integer,allocatable,dimension(:,:) :: n_tdata
      character(len=8),allocatable,dimension(:) :: stdid
      real(4),allocatable,dimension(:,:) :: cid
      real(4),allocatable,dimension(:,:,:) :: tdata_no3,tdata_no2

      character(len=8),dimension(1000) :: stdid_bias,stdid_rej,stdid_qrej,stdid_oth

      
     character(5) dtype,tm
     character(8) stid 
     character(1) ddtype
     character(100) file,file2,file3,file4,file5
     logical bias,rej,qrej,oth
     
      file=trim(dtype)//'_station'
      file2=trim(dtype)//'_bias_list'
      file3=trim(dtype)//'_rej_list'



     print *, dtype,nk

   

     open(11,file=file,form='unformatted')
     rewind(11)
     read(11) ndata,nread,n_miss_crit 

     allocate(tdata_no3(ndata,nk,nread),tdata_no2(ndata,nk,3),n_tdata(ndata,nk),stdid(ndata),cid(ndata,6))

     read(11) stdid(1:ndata)
     read(11) cid
     read(11) n_tdata(1:ndata,1:nk)
     read(11) tdata_no2(1:ndata,1:nk,:)
     read(11) tdata_no3(1:ndata,1:nk,1:nread)

    
     ddtype=dtype(1:1)
        call read_list2formake(dtype,file2,nd,1000,ncount_bias,stdid_bias)
        call read_list2formake(dtype,file3,nd,1000,ncount_rej,stdid_rej)

          close(11)

        file=trim(dtype)//'_stationlist'

        open(21,file=file,form='formatted')

        do i=1,ndata
            bias= .false.
            rej= .false.
            qrej= .false.
            oth= .false.
            stid=stdid(i)
            ddtype=stid(8:8)
            if(trim(ddtype) == 'a' .or. trim(ddtype) == 'x') then
               stid=stid(1:7)
               stdid(i)=trim(stid)
            endif
!            print *, 'stdid=',stdid(i)
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
!               print *,'bias=',bias
            endif
            if(ncount_rej >0 .and. .not. bias)  then
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
!               print *,'rej=',rej
            endif
            if(ncount_qrej >0 .and. .not. bias .and. .not. rej)  then
               do j=1,ncount_qrej
                  stid=stdid_qrej(j)
                  ddtype=stid(8:8)
                  if(trim(ddtype) == 'a' .or. trim(ddtype) == 'x') then
                     stdid_qrej(j)=stid(1:7)
                  endif
                  if(trim(stdid(i)) == trim(stdid_qrej(j)) ) then
                     write(21,200) stdid(i),cid(i,2),cid(i,1)
                     qrej=.true.
                     exit
                  endif
               enddo
!               print *,'qrej=',qrej
            endif
            if(ncount_oth >0 .and. .not. bias .and. .not. rej .and. &
                    .not. qrej ) then
               do j=1,ncount_oth
                  stid=stdid_oth(j)
                  ddtype=stid(8:8)
                  if(trim(ddtype) == 'a' .or. trim(ddtype) == 'x') then
                     stdid_oth(j)=stid(1:7)
                  endif
                  if(trim(stdid(i)) == trim(stdid_oth(j)) ) then
                     write(21,250) stdid(i),cid(i,2),cid(i,1)
                     oth=.true.
                     exit
                  endif
               enddo
!               print *,'oth=',oth
            endif
            if(  .not. bias .and. .not. rej .and. .not. qrej .and. &
                    .not. oth ) then
               write(21,300) stdid(i),cid(i,2),cid(i,1)
            endif
!            print *, bias,rej,qrej,oth
         enddo


100    format(a8,';',f8.2,';',f8.2,';  bias')
150    format(a8,';',f8.2,';',f8.2,';  reje')
200    format(a8,';',f8.2,';',f8.2,';  qrej')
250    format(a8,';',f8.2,';',f8.2,';  othe')
300    format(a8,';',f8.2,';',f8.2,';  norm')

        close(21)
        return 
        end
