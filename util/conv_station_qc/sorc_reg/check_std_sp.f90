!   read statistics files from different cycle delay (tm00,tm03 ..)
!   compare their values  


      subroutine check_std_sp(dtype,tm,filetype,nd,nd2,nk,stdid_input,rmean_input,std_input,&
                 std_no_input,xmed_input,break_input,tno_input,iqcinput,nlev,&
                 rmean_out,std_out,xmean_out,break_out,std_no_out,&
                 tno_out,iqcout,std_exist,list_true) 

       character(5) dtype,filetype
       character(2) tm
       character(1) ddtype
       character(8) stdid_input
       character(50) file1,file2
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
 
     ddtype=dtype(1:1)
     ddtype=trim(ddtype)

     print *,'check_std_sp:', tm,dtype,filetype

       call get_crit_sp(dtype,filetype,ct,nk)


     file1=trim(dtype)//'_station_'//trim(tm)
     file2=trim(dtype)//'_stas_'//trim(tm)
     open(11,file=file1,form='unformatted')
     rewind(11)
     read(11) ndata,nread,n_miss_crit

     print *,'check_std_sp: ndata,nread,n_miss_crit',ndata,nread,n_miss_crit

      allocate(stdid_s(ndata),cid_s(ndata,6),tno2_s(ndata,nk,3),xstas_s(ndata,nk,5),break_s(ndata,nk,5))
      allocate(tno3_s(ndata,nk,nread),n_tdata_s(ndata,nk))
 
     if(trim(ddtype) == 'u') then
        allocate(std_s(ndata,nk,6),rmean_s(ndata,nk,6),std_no_s(ndata,nk,6))
        n=6
     else
        allocate(std_s(ndata,nk,5),rmean_s(ndata,nk,5),std_no_s(ndata,nk,5))
        n=5
     endif

     print *,'check_std_sp:n,ddtype=', ddtype,n,file1,file2
  
     read(11) stdid_s(1:ndata)
     read(11) cid_s(1:ndata,:)
     read(11) n_tdata_s(1:ndata,1:nk)
     read(11) tno2_s(1:ndata,1:nk,1:3)
     read(11) tno3_s(1:ndata,1:nk,1:nread)

     open(21,file=file2,form='unformatted')
    rewind(21)
    do k=1,nk
       read(21) rmean_s(1:ndata,k,:)
    enddo

     do k=1,nk
         read(21) std_s(1:ndata,k,:)
     enddo

     do k=1,nk
         read(21) xstas_s(1:ndata,k,:)
     enddo

     do k=1,nk
         read(21) break_s(1:ndata,k,:)
     enddo
     do k=1,nk
        read(21) std_no_s(1:ndata,k,:)
     enddo


       close(11)
       close(21)

    

     
        if( trim(ddtype) == 'u') then
          ct=ct-10.0
        else
          ct=ct-0.2
        endif
        print *,'check_std_sp:ct=',ct(1),ct(1),ct(2),ct(3),ct(5),ct(10)
        print *,'check_std_sp:ndata=',ndata 
      
      do i=1,ndata
         if(trim(stdid_s(i)) == trim(stdid_input) ) then
            kcount=0
            do k=1,nk
               if(std_no_s (i,k,1) >5.0) kcount=kcount+1
            enddo 
            if( nlev <=2*kcount) then       
               std_exist=.true.
            endif
            print *, '1,'     
            if(trim(filetype) == 'bias') then
               kcount=0
               do k=1,nk
                  if(rmean_input(k,3) >-900.0) then
                     if(abs(rmean_input(k,3)) > abs(rmean_s(i,k,3)) & 
                        .and. abs(rmean_s(i,k,3)) >= ct(k)) then 
                        kcount=kcount+1
                     else if(abs(rmean_input(k,3)) <abs(rmean_s(i,k,1)) ) then
                        kcount=kcount+1
                     endif
                  endif 
               enddo
            print *, '2,',kcount     
            
               if(nlev <=2*kcount) then
                  list_true=.true. 
               endif
               if(list_true) then
                  do k=1,nk
                     if(rmean_input(k,3) >-900.0) then
                        if(abs(rmean_input(k,3)) > abs(rmean_s(i,k,3)) &
                           .and. abs(rmean_s(i,k,3)) >= ct(k)) then
                           rmean_out(k,1:nd) = rmean_s(i,k,1:nd)
                           std_out(k,1:nd) = std_s(i,k,1:nd)
                           xmean_out(k,1:nd2) = xstas_s(i,k,1:nd2)
                           break_out(k,1:nd2) = break_s(i,k,1:nd2)
                           std_no_out(k)=std_no_s(i,k,1)
                           tno_out(k,1:2)=tno2_s(i,k,1:2)
                           iqcout(k) =0
                        else if(abs(rmean_input(k,3)) < abs(rmean_s(i,k,3))) then
                           rmean_out(k,1:nd) = rmean_input(k,1:nd)
                           std_out(k,1:nd) = std_input(k,1:nd)
                           xmean_out(k,1:nd2) = xmed_input(k,1:nd2)
                           break_out(k,1:nd2) = break_input(k,1:nd2)
                           std_no_out(k)=std_no_input(k)
                           tno_out(k,1:2)=tno_input(k,1:2)
                           iqcout(k) =0
                        else if(abs(rmean_input(k,3)) > abs(rmean_s(i,k,3)) .and. &
                           abs(rmean_s(i,k,3))  <ct(k)) then
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
               print *,'check_std_sp: fieltype=rej,1'
               kcount=0
               do k=1,nk
                  if(rmean_input(k,3) >-900.0) then
                     if(std_input(k,3) > std_s(i,k,3) &
                        .and. abs(std_s(i,k,3)) >= ct(k)) then
                        kcount=kcount+1
                     else if(std_input(k,3) <std_s(i,k,3) ) then
                        kcount=kcount+1
                     endif
                  endif
               enddo
               if(nlev <=2*kcount) then
                  list_true=.true.
               endif
               print *,'check_std_sp:,kcount,nlev=',kcount,nlev,list_true
               if(list_true) then
                  do k=1,nk
                     if(rmean_input(k,3) >-900.0) then
                        if(std_input(k,3) > std_s(i,k,3) &
                           .and. std_s(i,k,3) >= ct(k)) then
                           rmean_out(k,1:nd) = rmean_s(i,k,1:nd)
                           std_out(k,1:nd) = std_s(i,k,1:nd)
                           xmean_out(k,1:nd2) = xstas_s(i,k,1:nd2)
                           break_out(k,1:nd2) = break_s(i,k,1:nd2)
                           std_no_out(k)=std_no_s(i,k,1)
                           tno_out(k,1:2)=tno2_s(i,k,1:2)
                           iqcout(k)=iqcinput(k)
                        else if(std_input(k,3) < std_s(i,k,3)) then
                           rmean_out(k,1:nd) = rmean_input(k,1:nd)
                           std_out(k,1:nd) = std_input(k,1:nd)
                           xmean_out(k,1:nd2) = xmed_input(k,1:nd2)
                           break_out(k,1:nd2) = break_input(k,1:nd2)
                           std_no_out(k)=std_no_input(k)
                           tno_out(k,1:2)=tno_input(k,1:2)
                           iqcout(k)=iqcinput(k)
                        else if(std_input(k,3) > std_s(i,k,3) .and.&
                           std_s(i,k,3) <ct(k)) then
                          print *,'check_std_sp:,rmean_out,3',rmean_out(k,1),rmean_input(k,1)
                           rmean_out(k,1:nd) = -999.0
                           std_out(k,1:nd) = -999.0
                           xmean_out(k,1:nd2) = -999.0
                           break_out(k,1:nd2) = -999.0
                           std_no_out(k)=0.0
                           tno_out(k,1:2)=0.0
                           iqcout(k)=0
                        endif
                     endif
                  enddo
               endif
            exit
            endif
         endif
      enddo

      deallocate(stdid_s,cid_s,std_s,rmean_s,std_no_s,tno2_s,tno3_s,xstas_s,break_s,n_tdata_s)

 
      print *, 'check_std_sp:',tm,stdid_input,std_exist,list_true,nlev,kcount

     return
     end
