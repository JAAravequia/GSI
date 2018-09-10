!  the program set criteria for potential rejection list of stations and bias list
!  of the surface pressure stations



        subroutine read_list(dtype,nk)


     character(len=8),allocatable,dimension(:) :: stdid
     real(4),allocatable,dimension(:,:,:) :: std,std_no,rmean,break
    real(4),allocatable,dimension(:,:) :: cid
    real(4),allocatable,dimension(:,:,:) :: tdata_no3,tdata_no2,xstas
    integer,allocatable,dimension(:,:) :: n_tdata,n2_tdata

     
     real(4),dimension(43) :: plevel
     real(4),dimension(6) :: plev_man

     
     character(5) dtype
     character(8)  stid
     character(50) file1,file2
     logical lexist

     real(4) rtim,nflg0,xlat0,xlon0,nlev0,rlat,rlon
    
     data plevel/1000.,975.,950.,925.,900.,875.,850.,825.,800.,775.,750.,725.,700.,675.,650.,625.,600.,575.,550.,525.,500.,475.,450.,425.,400.,375.,350.,325.,300.,275.,250.,225.,200.,175.,150.,125.,100.,70.,50.,30.,20.,10.,5./


     data plev_man /1000.,925.,850.,700.,500.,300./


     !  open input file and file names

    file1=trim(dtype)//'_station'
    inquire (file=file1,exist=lexist)
     if(.not. lexist) then
       print *,'read_list:the file doesnot exist ',file1
       return
     endif


     open(11,file=file1,form='unformatted')
   
     rewind(11)
     read(11) ndata,nread,n_miss_crit

   allocate(stdid(ndata),cid(ndata,6))
   allocate(tdata_no2(ndata,nk,3))
   allocate(n_tdata(ndata,nk),tdata_no3(ndata,nk,nread))
   allocate(std(ndata,nk,5),std_no(ndata,nk,5),rmean(ndata,nk,5))
   allocate(xstas(ndata,nk,5),break(ndata,nk,5))
   allocate(xmed(ndata,nk,2),smad(ndata,nk,2))
    allocate(index(ndata,nk),index_rej(ndata,nk),index_bias(ndata,nk))
    allocate(index_other(ndata,nk),index_qly(ndata,nk),n2_tdata(ndata,nk))
    allocate(index2(ndata),index2_rej(ndata),index2_bias(ndata))
    allocate(index2_other(ndata),index2_qly(ndata),index2_norm(ndata))

!     read(11) stdid(1:ndata),cid(1:ndata,1:6)
!     read(11) n_tdata(1:ndata,1:nk)
!     read(11) tdata_no2(1:ndata,1:nk,1:3)
!     read(11) tdata_no3(1:ndata,1:nk,1:nread)

      read(11) stdid
      read(11) cid
      read(11) n_tdata
      read(11) tdata_no2
      read(11) tdata_no3
 

      write(6,*) 'ndata=',ndata
      write(6,500) (stdid(i),i=1088,1098)
      write(6,510) (cid(i,1),i=1088,1098)
      write(6,520) (n_tdata(i,3),i=1088,1098)

500 format(10a8)
510 format(10f8.2)
520 format(10i8)

!  open statistics file

    print *,'start to read stas file'

     file2=trim(dtype)//'_stas'

      open(21,file=file2,form='unformatted')

      rewind(21)

      do k=1,nk
       read(21) rmean(1:ndata,k,:)
     enddo

     do k=1,nk
         read(21) std(1:ndata,k,:)
      enddo

     do k=1,nk
         read(21) xstas(1:ndata,k,:)
      enddo

     do k=1,nk
         read(21) break(1:ndata,k,:)
      enddo
     do k=1,nk
         read(21) std_no(1:ndata,k,:)
      enddo


 
       close(11)
       close(21)  

        print *,'end of stats file'

       write(6,600) (rmean(i,5,1),i=1,10)
      write(6,600) (std(i,6,1),i=1,10)
      write(6,600) (break(i,7,1),i=1,10)
600 format(10f8.2)


   deallocate(stdid,cid,tdata_no2,n_tdata,tdata_no3,std,std_no,rmean,xstas)
   deallocate(break,xmed,smad)
    return
    end
