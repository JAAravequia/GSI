!   read statistics files from different cycle delay (tm00,tm03 ..)
!   compare their values  


      subroutine check_std_sfc(dtype,tm,filetype,nd,nd2,stdid_input,rmean_input,std_input,&
                 std_no_input,xmed_input,break_input,tno_input,iqcinput,&
                 rmean_out,std_out,xmean_out,break_out,std_no_out,&
                 tno_out,iqcout,std_exist,list_true) 

       character(5) dtype,filetype
       character(2) tm
       character(1) ddtype
       character(8) stdid_input
       character(50) file1,file2
       character(len=8),allocatable,dimension(:) :: stdid_s
       real(4),allocatable,dimension(:,:) :: cid_s,rmean_s,xstas_s,break_s,std_s
       real(4),allocatable,dimension(:,:) :: std_no_s,tno2_s,tno3_s
       integer,allocatable,dimension(:) :: n_tdata_s

       real(4),dimension(2) :: tno_out
       real(4),dimension(nd) :: std_out,rmean_out
       real(4),dimension(nd2) :: xmean_out,break_out 
       real(4),dimension(2) :: tno_input
       real(4),dimension(nd) :: std_input,rmean_input
       real(4),dimension(nd2) :: xmed_input,break_input 

       real(4) std_no_out,std_no_input
       

        logical std_exist,list_true,lexist

        std_exist=.false.
        list_true=.false.
        rmean_out=rmean_input
        std_out=std_input
        xmean_out=xmed_input
        break_out=break_input
        std_no_out=std_no_input
        tno_out=tno_input 
        iqcout=iqcinput
       
     ddtype=dtype(1:1)
     ddtype=trim(ddtype)

  if(iqcinput ==2) then
     std_exist=.true. 
     list_true=.true.
     return
  else
     call get_crit_sfc(dtype,filetype,ct)
  endif

!     print *,'check_std_sfc:tm,dtype,filetype=', tm,dtype,filetype

     file1=trim(dtype)//'_station_'//trim(tm)
     file2=trim(dtype)//'_stas_'//trim(tm)

     inquire ( file=file1,exist=lexist)
     if(.not. lexist) then
       print *,'check_std_sfc:the file doesnot exist ',file1
       return
     endif

     inquire ( file=file2,exist=lexist)
     if(.not. lexist) then
       print *,'check_std_sfc:the file doesnot exist ',file2
       return
     endif

     open(11,file=file1,form='unformatted')
     rewind(11)
     read(11) ndata,nread,n_miss_crit

!     print *,'check_std_sfc:ndata,nread,n_miss_crit',ndata,nread,n_miss_crit

      allocate(stdid_s(ndata),cid_s(ndata,6),tno2_s(ndata,3),xstas_s(ndata,5),break_s(ndata,5))
      allocate(tno3_s(ndata,nread),n_tdata_s(ndata))
 
     if(trim(ddtype) == 'u') then
        allocate(std_s(ndata,6),rmean_s(ndata,6),std_no_s(ndata,6))
        n=6
     else
        allocate(std_s(ndata,5),rmean_s(ndata,5),std_no_s(ndata,5))
        n=5
     endif
     read(11) stdid_s(1:ndata),cid_s(1:ndata,:)
     read(11) n_tdata_s(1:ndata)
     read(11) tno2_s(1:ndata,:)
     read(11) tno3_s(1:ndata,1:nread)

     open(21,file=file2,form='unformatted')
    rewind(21)
    read(21) rmean_s(1:ndata,:)
    read(21) std_s(1:ndata,:)
    read(21) xstas_s(1:ndata,:)
    read(21) break_s(1:ndata,:)
    read(21) std_no_s(1:ndata,:)

       close(11)
       close(21)

    

        ct=ct-0.2
!        print *,'check_std_sfc:ct=',ct
      
      do i=1,ndata
         if(trim(stdid_s(i)) == trim(stdid_input) .and. std_no_s (i,1) >5.0) then
            std_exist=.true.
            print *, '1,',rmean_input(1),rmean_s(i,1),std_input(1),std_s(i,1)     
            if(trim(filetype) == 'bias') then
               if(abs(rmean_input(1)) > abs(rmean_s(i,1)) .and. &
                   abs(rmean_s(i,1)) >= ct) then 
                  print *,'2,check_std_sfc:',(break_s(i,j),j=1,4)
                  list_true=.true. 
                  rmean_out(1:nd) = rmean_s(i,1:nd)
                  std_out(1:nd) = std_s(i,1:nd)
                  xmean_out(1:nd2) = xstas_s(i,1:nd2)
                  break_out(1:nd2) = break_s(i,1:nd2)
                  std_no_out=std_no_s(i,1)
                  tno_out(1:2)=tno2_s(i,1:2)
                  iqcout=iqcinput
                  print *,'2,check_std_sfc:',(break_out(j),j=1,4)
               else if( abs(rmean_input(1)) <abs(rmean_s(i,1))) then
                  list_true=.true. 
                  print *,'3,check_std_sfc:',(break_s(i,j),j=1,4)
                  rmean_out(1:nd) = rmean_input(1:nd)
                  std_out(1:nd) = std_input(1:nd)
                  xmean_out(1:nd2) = xmed_input(1:nd2)
                  break_out(1:nd2) = break_input(1:nd2)
                  std_no_out=std_no_input
                  tno_out(1:2)=tno_input(1:2)
                  iqcout=iqcinput
                  print *,'3,check_std_sfc:',(break_out(j),j=1,4),tno_out(1),tno_out(2)
               endif
            else if(trim(filetype) == 'rej') then
               if(std_input(1) > std_s(i,1) .and. std_s(i,1) >= ct ) then 
                  list_true=.true. 
                  rmean_out(1:nd) = rmean_s(i,1:nd)
                  std_out(1:nd) = std_s(i,1:nd)
                  xmean_out(1:nd2) = xstas_s(i,1:nd2)
                  break_out(1:nd2) = break_s(i,1:nd2)
                  std_no_out=std_no_s(i,1)
                  tno_out(1:2)=tno2_s(i,1:2)
                  iqcout=iqcinput
            print *, '4,',std_input(1),std_s(i,1),ct     
               else if (std_input(1)  < std_s(i,1) ) then
                  list_true=.true.
                  rmean_out(1:nd) = rmean_input(1:nd)
                  std_out(1:nd) = std_input(1:nd)
                  xmean_out(1:nd2) = xmed_input(1:nd2)
                  break_out(1:nd2) = break_input(1:nd2)
                  std_no_out=std_no_input
                  tno_out(1:2)=tno_input(1:2)
                  iqcout=iqcinput
            print *, '5,',std_input(1),std_s(i,1),ct     
               endif
            endif   
            exit
         endif
      enddo

      deallocate(stdid_s,cid_s,std_s,rmean_s,std_no_s,tno2_s,tno3_s,xstas_s,break_s,n_tdata_s)

 
!      print *, 'check_std_sfc:',stdid_input,std_exist,list_true,rmean_out(1),std_out(1)

     return
     end
