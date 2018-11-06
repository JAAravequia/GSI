!  the program set criteria for potential rejection list of stations and bias list
!  of the surface pressure stations



        subroutine read_list_sfc(dtype,file1,file2,n,ndata,stdid,cid,tdata_no2,std,rmean,std_no)

!!!    get criteria from crite_constnt module
  
     

      real(4),allocatable,dimension(:,:) :: break,xstas
      real(4),allocatable,dimension(:,:) :: xmed,smad,tdata_no3,stno 
      integer,allocatable,dimension(:) :: n_tdata,n2_tdata

      character(len=8),dimension(ndata) :: stdid
      real(4),dimension(ndata,n) :: std,rmean
      real(4),dimension(ndata,6) :: cid
      real(4),dimension(ndata) :: std_no
      real(4),dimension(ndata,3) :: tdata_no2
     
     character(5) dtype
     character(8)  stid
     character(100) file1,file2





     open(11,file=file1,form='unformatted')
     rewind(11)
     read(11) ndata,nread,n_miss_crit 

     allocate(tdata_no3(ndata,nread),n_tdata(ndata),n2_tdata(ndata),stno(ndata,n))
     allocate(break(ndata,5))
     allocate(xstas(ndata,5))

     read(11) stdid(1:ndata),cid(1:ndata,:)
     read(11) n_tdata(1:ndata)
     read(11) tdata_no2(1:ndata,:)
     read(11) tdata_no3(1:ndata,1:nread)


    open(21,file=file2,form='unformatted')
    rewind(21)
    read(21) rmean(1:ndata,:)
       read(21) std(1:ndata,:)
       read(21) xstas(1:ndata,:)
       read(21) break(1:ndata,:)
       read(21) stno(1:ndata,:)
       
       std_no(1:ndata)=stno(1:ndata,1)

    deallocate(tdata_no3,n_tdata,n2_tdata,break,xstas,stno)

          close(11)
          close(21)


        return
        end
