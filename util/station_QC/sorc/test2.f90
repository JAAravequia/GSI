     character(len=8),allocatable,dimension(:) :: stdid
     real(4),allocatable,dimension(:,:,:) :: std,std_no,rmean,break,xmed,xb,sb,smad
    real(4),allocatable,dimension(:,:) :: cid
    real(4),allocatable,dimension(:,:,:) :: tdata_no3,tdata_no2,xstas
    real(4),allocatable,dimension(:,:,:,:) :: tdata_no
    integer,allocatable,dimension(:,:) :: n_tdata

    nk=43
    open(11,file='t120_station',form='unformatted')

     rewind(11)
     read(11) ndata,nread,n_miss_crit

     allocate(stdid(ndata),cid(ndata,6))
   allocate(tdata_no(ndata,nk,2,nread),tdata_no2(ndata,nk,3))
   allocate(n_tdata(ndata,nk),tdata_no3(ndata,nk,nread))
   allocate(std(ndata,nk,5),std_no(ndata,nk,5),rmean(ndata,nk,5))
   allocate(xstas(ndata,nk,5),break(ndata,nk,5))
   allocate(xmed(ndata,nk,2),xb(ndata,nk,2),sb(ndata,nk,2),smad(ndata,nk,2))


      read(11) stdid(1:ndata)
      read(11) cid(1:ndata,1:6) 
      read(11) n_tdata(1:ndata,1:nk)
      read(11) tdata_no2(1:ndata,1:nk,1:3)
      read(11) tdata_no3(1:ndata,1:nk,1:nread)


       write(6,*) 'ndata=',ndata
      write(6,500) (stdid(i),i=1088,1098)
      write(6,510) (cid(i,1),i=1088,1098)
      write(6,520) (n_tdata(i,3),i=1088,1098)

500 format(10a8)
510 format(10f8.2)
520 format(10i8)



        stop
        end
