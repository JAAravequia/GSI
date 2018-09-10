!   read statistics files from different cycle delay (tm00,tm03 ..)
!   compare their values  


      subroutine check_std(dtype,tm,filetype,nd,nd2,nk,stdid_input,rmean_input,std_input,&
                 std_no_input,xmed_input,break_input,tno_input,iqcinput,nlev,&
                 rmean_out,std_out,xmean_out,break_out,std_no_out,&
                 tno_out,iqcout,std_exist,list_true) 

       character(5) dtype,filetype
       character(2) tm
       character(1) ddtype
       character(8) stdid_input
       character(25) file1,file2
       character(len=8),allocatable,dimension(:) :: stdid_s
       real(4),allocatable,dimension(:,:) :: cid_s
       real(4),allocatable,dimension(:,:,:) :: rmean_s,xstas_s,break_s,std_s
       real(4),allocatable,dimension(:,:,:) :: std_no_s,tno2_s,tno3_s
       integer,allocatable,dimension(:,:) :: n_tdata_s

       real(4),dimension(nk,2) :: tno_out
       real(4),dimension(nk,nd) :: std_out,rmean_out
       real(4),dimension(nk,nd2) :: xmean_out,break_out 
       real(4),dimension(nk,2) :: tno_input
       real(4),dimension(nk,nd) :: std_input,rmean_input
       real(4),dimension(nk,nd2) :: xmed_input,break_input 
       real(4),dimension(nk) :: ct,std_no_out,std_no_input
       integer,dimension(nk) :: iqcinput,iqcout

       

        logical std_exist,list_true

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

     print *,'check_std:', tm,dtype,filetype

       call get_crit(dtype,filetype,ct,nk)


     file1=trim(dtype)//'_station_'//trim(tm)
     file2=trim(dtype)//'_stas_'//trim(tm)
     open(11,file=file1,form='unformatted')
     rewind(11)
     read(11) nob,nread,n_miss_crit

!     print *,'check_std: nob,nread,n_miss_crit',nob,nread,n_miss_crit

      allocate(stdid_s(nob),cid_s(nob,6),tno2_s(nob,nk,3),xstas_s(nob,nk,5),break_s(nob,nk,5))
      allocate(tno3_s(nob,nk,nread),n_tdata_s(nob,nk))
 
     if(trim(ddtype) == 'u') then
        allocate(std_s(nob,nk,6),rmean_s(nob,nk,6),std_no_s(nob,nk,6))
        n=6
     else
        allocate(std_s(nob,nk,5),rmean_s(nob,nk,5),std_no_s(nob,nk,5))
        n=5
     endif

     print *,'check_std:n,ddtype=', ddtype,n,file1,file2
  
     read(11) stdid_s(1:nob)
     read(11) cid_s(1:nob,:)
     read(11) n_tdata_s(1:nob,1:nk)
     read(11) tno2_s(1:nob,1:nk,1:3)
     read(11) tno3_s(1:nob,1:nk,1:nread)

     open(21,file=file2,form='unformatted')
    rewind(21)
    do k=1,nk
       read(21) rmean_s(1:nob,k,:)
    enddo

     do k=1,nk
         read(21) std_s(1:nob,k,:)
     enddo

     do k=1,nk
         read(21) xstas_s(1:nob,k,:)
     enddo

     do k=1,nk
         read(21) break_s(1:nob,k,:)
     enddo
     do k=1,nk
        read(21) std_no_s(1:nob,k,:)
     enddo


       close(11)
       close(21)

    

     
        if( trim(ddtype) == 'u') then
          ct=ct-10.0
        else
          ct=ct-0.2
        endif
        print *,'check_std:nob=',nob 

       do i=1,nob
         write(6,200) stdid_s(i)
       enddo    

200 format (10a9)
        print *,'check_std:nob=',nob 
      
      do i=1,nob
         if(trim(stdid_s(i)) == trim(stdid_input) ) then
            kcount=0
            do k=1,nk
               if(std_no_s (i,k,1) >5.0) kcount=kcount+1
            enddo 
            if( nlev <=2*kcount) then       
               std_exist=.true.
            endif
            print *, 'check_std:std_exist=',std_exist,filetype     
            if(trim(filetype) == 'bias') then
               kcount=0
               do k=1,nk
                  if(rmean_input(k,1) >-900.0) then
                     if(abs(rmean_input(k,1)) > abs(rmean_s(i,k,1)) & 
                        .and. abs(rmean_s(i,k,1)) >= ct(k)) then 
                        kcount=kcount+1
                     else if(abs(rmean_input(k,1)) <abs(rmean_s(i,k,1)) ) then
                        kcount=kcount+1
                     endif
                  endif 
               enddo
!            print *, '2,',kcount     
            
               if(nlev <=2*kcount) then
                  list_true=.true. 
               endif
               if(list_true) then
                  do k=1,nk
                     if(rmean_input(k,1) >-900.0) then
               print *,'check_std: fieltype=bias,1'
                        if(abs(rmean_input(k,1)) > abs(rmean_s(i,k,1)) &
                           .and. abs(rmean_s(i,k,1)) >= ct(k)) then
                           rmean_out(k,1:nd) = rmean_s(i,k,1:nd)
                           std_out(k,1:nd) = std_s(i,k,1:nd)
                           xmean_out(k,1:nd2) = xstas_s(i,k,1:nd2)
                           break_out(k,1:nd2) = break_s(i,k,1:nd2)
                           std_no_out(k)=std_no_s(i,k,1)
                           tno_out(k,1:2)=tno2_s(i,k,1:2)
                           iqcout(k) =0
                        else if(abs(rmean_input(k,1)) < abs(rmean_s(i,k,1))) then
                           rmean_out(k,1:nd) = rmean_input(k,1:nd)
                           std_out(k,1:nd) = std_input(k,1:nd)
                           xmean_out(k,1:nd2) = xmed_input(k,1:nd2)
                           break_out(k,1:nd2) = break_input(k,1:nd2)
                           std_no_out(k)=std_no_input(k)
                           tno_out(k,1:2)=tno_input(k,1:2)
                           iqcout(k) =0
                        else if(abs(rmean_input(k,1)) > abs(rmean_s(i,k,1)) .and. &
                           abs(rmean_s(i,k,1))  <ct(k)) then
                           rmean_out(k,1:nd) = -999.0 
                           std_out(k,1:nd) = -999.0 
                           xmean_out(k,1:nd2) = -999.0 
                           break_out(k,1:nd2) = -999.0 
                           std_no_out(k)=0.0
                           tno_out(k,1:2)=0.0
                           iqcout(k) =0
                        endif
                     endif
                  enddo
               endif
            else if(trim(filetype) == 'rej') then
               print *,'check_std: fieltype=rej,1'
               kcount=0
               do k=1,nk
                  if(rmean_input(k,1) >-900.0) then
                     if(std_input(k,1) > std_s(i,k,1) &
                        .and. abs(std_s(i,k,1)) >= ct(k)) then
                        kcount=kcount+1
                     else if(std_input(k,1) <std_s(i,k,1) ) then
                        kcount=kcount+1
                     endif
                  endif
               enddo
               if(nlev <=2*kcount) then
                  list_true=.true.
               endif
               print *,'check_std:,kcount,nlev=',kcount,nlev,list_true
               if(list_true) then
                  do k=1,nk
                     if(rmean_input(k,1) >-900.0) then
                        if(std_input(k,1) > std_s(i,k,1) &
                           .and. std_s(i,k,1) >= ct(k)) then
                           rmean_out(k,1:nd) = rmean_s(i,k,1:nd)
                           std_out(k,1:nd) = std_s(i,k,1:nd)
                           xmean_out(k,1:nd2) = xstas_s(i,k,1:nd2)
                           break_out(k,1:nd2) = break_s(i,k,1:nd2)
                           std_no_out(k)=std_no_s(i,k,1)
                           tno_out(k,1:2)=tno2_s(i,k,1:2)
                           iqcout(k)=iqcinput(k)
                           print *, 'check_std:iqc=',iqcout(k),iqcinput(k)
                        else if(std_input(k,1) < std_s(i,k,1)) then
                           rmean_out(k,1:nd) = rmean_input(k,1:nd)
                           std_out(k,1:nd) = std_input(k,1:nd)
                           xmean_out(k,1:nd2) = xmed_input(k,1:nd2)
                           break_out(k,1:nd2) = break_input(k,1:nd2)
                           std_no_out(k)=std_no_input(k)
                           tno_out(k,1:2)=tno_input(k,1:2)
                           iqcout(k)=iqcinput(k)
                           print *, 'check_std:iqc=',iqcout(k),iqcinput(k)
                        else if(std_input(k,1) > std_s(i,k,1) .and.&
                           std_s(i,k,1) <ct(k)) then
                          print *,'check_std:,rmean_out,3',rmean_out(k,1),rmean_input(k,1)
                           rmean_out(k,1:nd) = -999.0
                           std_out(k,1:nd) = -999.0
                           xmean_out(k,1:nd2) = -999.0
                           break_out(k,1:nd2) = -999.0
                           std_no_out(k)=0.0
                           tno_out(k,1:2)=0.0
                           iqcout(k)=iqcinput(k)
                        endif
                     endif
                  enddo
               endif
            exit
            endif
         endif
      enddo

      deallocate(stdid_s,cid_s,std_s,rmean_s,std_no_s,tno2_s,tno3_s,xstas_s,break_s,n_tdata_s)

 
!      print *, 'check_std:',tm,stdid_input,std_exist,list_true,nlev,kcount

     return
     end
