!   read statistics files from different cycle delay (tm00,tm03 ..)
!   compare their values  


      subroutine check_data_sfc_ttm3(ntm,dtype,filetype,tm,filetm,n,nd,nd2,stdid,&
                 cid,rmean,std,std_no,tno,xmed,break,iqcflg,ncount) 

       character(2) tm,tm1,tm2,tm3,tm4
       character(5) dtype,filetype
       character(1) ddtype
       character(8) stdid_input

      character(2),dimension(ntm-1) :: filetm
      character(8),dimension(n) :: stdid,stdid_00
      real(4),dimension(n,2) :: cid,cid_00
      real(4),dimension(n,nd) :: rmean,rmean_00
      real(4),dimension(n,nd) :: std,std_00
      real(4),dimension(n) :: std_no,std_no_00
      real(4),dimension(n,2) :: tno,tno_00
      real(4),dimension(n,nd2) :: xmed,xmed_00 
      real(4),dimension(n,nd2) :: break,break_00 
      real(4),dimension(nd) :: std_out,rmean_out,std_out2,rmean_out2
      real(4),dimension(2) :: tno_out,tno_out2
      real(4),dimension(2) :: tno_input
      real(4),dimension(nd) :: std_input,rmean_input
      real(4),dimension(nd2) :: xmed_out,xmed_out2,xmed_input 
      real(4),dimension(nd2) :: break_out,break_input 
      real(4),dimension(nd2) :: break_out2 

      integer,dimension(n) :: iqcflg,iqcflg00

      logical std_exist,std2_exist,std3_exist,std4_exist 
      logical list_true,list2_true,list3_true,list4_true 

        character(50) file1,file2,file3,file4,file5


!!  read bias or rejection list 


     ddtype=dtype(1:1)
     std_exist=.false.
     std2_exist=.false.
     std3_exist=.false.
     std4_exist=.false.

     list_true=.false.
     list2_true=.false.
     list3_true=.false.
     list4_true=.false.

     print *,'check_data_sfc: dtype=',dtype,ddtype,tm
     if(ddtype == 'u') then
     file1=trim(dtype)//'_'//trim(filetype)//'_dir_list_'//trim(tm)
       call read_listw_sfc(dtype,file1,n,ndata,nd,nd2,stdid_00,cid_00,rmean_00,&
                         std_00,xmed_00,break_00,std_no_00,tno_00,iqcflg00)
    else 
     file1=trim(dtype)//'_'//trim(filetype)//'_list_'//trim(tm)
       call read_list2_sfc(dtype,file1,n,ndata,nd,nd2,stdid_00,cid_00,rmean_00,&
                         std_00,xmed_00,break_00,std_no_00,tno_00,iqcflg00)
   endif

     print *,' check_data_sfc: ndata=',ndata
!      do i=1,ndata
!        write(6,100) stdid_00(i),cid_00(i,1),cid_00(i,2),rmean_00(i,1),rmean_00(i,2),&
!                     std_00(i,1),std_00(i,2),break_00(i,1),break_00(i,2),std_no_00(i)
!        write(6,106) break_00(i,3),break_00(i,4),xmed_00(i,1),xmed_00(i,2),xmed_00(i,3),&
!                     xmed_00(i,4),tno_00(i,1),tno_00(i,2)
!      enddo

!100  format(a8,9f7.2)
!106 format(8f7.2)

!  compare all tm list, get smallest rmean and std values for all tm

     if(ntm ==2) then
        tm1=filetm(1) 
     else if (ntm ==3) then
        tm1=filetm(1) 
        tm2=filetm(2)
     else if(ntm ==4) then 
        tm1=filetm(1) 
        tm2=filetm(2)
        tm3=filetm(3)
     endif

    print *,'check_data_sfc,tm,tm1,tm2,tm3=',tm,' ', tm1,' ',tm2,' ',tm3


  
  
     ncount=0
  do i=1,ndata                          !  tm 00
     if(iqcflg00(i) == 2) then
        ncount=ncount+1
        stdid(ncount) = stdid_00(i)
        cid(ncount,1:2) = cid_00(i,1:2)
        rmean(ncount,1:nd) = rmean_00(i,1:nd)
        std(ncount,1:nd) = std_00(i,1:nd)
        xmed(ncount,1:nd2) = xmed_00(i,1:nd2)
        break(ncount,1:nd2) = break_00(i,1:nd2)
        std_no(ncount) = std_no_00(i)
        tno(ncount,1:2)=tno_00(i,1:2)     
        iqcflg(ncount)=iqcflg00(i)
     else
        stdid_input=stdid_00(i)
        rmean_input(1:nd)=rmean_00(i,1:nd)
        std_input(1:nd)=std_00(i,1:nd)
        std_no_input=std_no_00(i)
        xmed_input(1:nd2)=xmed_00(i,1:nd2)
        break_input(1:nd2)=break_00(i,1:nd2)
        tno_input(1:2)=tno_00(i,1:2)
        iqcinput=iqcflg00(i)
        call check_std_sfc(dtype,tm1,filetype,nd,nd2,stdid_input,rmean_input,std_input, &  
                    std_no_input,xmed_input,break_input,tno_input,iqcinput,& 
                    rmean_out,std_out,xmed_out,break_out,&     
                    std_no_out,tno_out,iqcout,std_exist,list_true)
        if(std_exist .and. list_true ) then
              rmean_input(1:nd)=rmean_out(1:nd)
              std_input(1:nd)=std_out(1:nd)
              std_no_input=std_no_out
              xmed_input(1:nd2)=xmed_out(1:nd2)
              break_input(1:nd2)=break_out(1:nd2)
              tno_input(1:2)=tno_out(1:2)
              iqcinput=iqcout
!              print *,'1,check_data_sfc:',tm1,rmean_input(1),std_input(1),std_no_input
              call check_std_sfc(dtype,tm2,filetype,nd,nd2,stdid_input,rmean_input,std_input, &  
                    std_no_input,xmed_input,break_input,tno_input,iqcinput,& 
                    rmean_out2,std_out2,xmed_out2,break_out2,&     
                    std_no_out2,tno_out2,iqcout,std2_exist,list2_true)
           if(std2_exist .and. list2_true ) then
                 ncount=ncount+1
                 stdid(ncount) = stdid_00(i)
                 cid(ncount,1:2) = cid_00(i,1:2)
                 rmean(ncount,1:nd) = rmean_out2(1:nd)
                 std(ncount,1:nd) = std_out2(1:nd)
                 xmed(ncount,1:nd2) = xmed_out2(1:nd2)
                 break(ncount,1:nd2) = break_out2(1:nd2)
                 std_no(ncount) = std_no_out2
                 tno(ncount,1:2)=tno_out2(1:2)
                 iqcflg(ncount)=iqcout
           else if( .not. std2_exist) then 
                 ncount=ncount+1
                 stdid(ncount) = stdid_00(i)
                 cid(ncount,1:2) = cid_00(i,1:2)
                 rmean(ncount,1:nd) = rmean_out(1:nd)
                 std(ncount,1:nd) = std_out(1:nd)
                 xmed(ncount,1:nd2) = xmed_out(1:nd2)
                 break(ncount,1:nd2) = break_out(1:nd2)
                 std_no(ncount) = std_no_out
                 tno(ncount,1:2)=tno_out(1:2)
                 iqcflg(ncount)=iqcout
           endif
        else if( .not. std_exist) then
           call check_std_sfc(dtype,tm2,filetype,nd,nd2,stdid_input,rmean_input,std_input,&
                     std_no_input,xmed_input,break_input,tno_input,iqcinput,& 
                     rmean_out2,std_out2,xmed_out2,break_out2,&
                     std_no_out2,tno_out2,iqcout,std2_exist,list2_true)
           if(std2_exist .and. list2_true ) then
                 ncount=ncount+1
                 stdid(ncount) = stdid_00(i)
                 cid(ncount,1:2) = cid_00(i,1:2)
                 rmean(ncount,1:nd) = rmean_out2(1:nd)
                 std(ncount,1:nd) = std_out2(1:nd)
                 xmed(ncount,1:nd2) = xmed_out2(1:nd2)
                 xmed(ncount,1:nd2) = xmed_out2(1:nd2)
                 break(ncount,1:nd2) = break_out2(1:nd2)
                 std_no(ncount) = std_no_out2
                 tno(ncount,1:2)=tno_out2(1:2)
                 iqcflg(ncount)=iqcout
           else if( .not.std2_exist) then
                 ncount=ncount+1
                 stdid(ncount) = stdid_00(i)
                 cid(ncount,1:2) = cid_00(i,1:2)
                 rmean(ncount,1:nd) = rmean_00(i,1:nd)
                 std(ncount,1:nd) = std_00(i,1:nd)
                 xmed(ncount,1:nd2) = xmed_00(i,1:nd2)
                 break(ncount,1:nd2) = break_00(i,1:nd2)
                 std_no(ncount) = std_no_00(i)
                 tno(ncount,1:2)=tno_00(i,1:2)
                 iqcflg(ncount)=iqcout
           endif
        endif
     endif
  enddo            

    print *,'check_data_sfc: tm,ncount=',tm,ncount 
!             do i=1,ncount
!                write(6,200) stdid(i),cid(i,1),cid(i,2),rmean(i,1),rmean(i,2),&
!                std(i,1),std(i,2),std_no(i),tno(i,1),tno(i,2),iqcflg(i)
!                write(6,210) break(i,1),break(i,2),break(i,3),break(i,4),&
!                             xmed(i,1),xmed(i,2),xmed(i,3),xmed(i,4)
!      enddo

!200  format(a8,9f7.2,i3)
!210   format(8f7.2)



              return
              end


