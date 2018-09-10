!    read the bias and rejection list

     
     subroutine readwrite_list(dtype,filetype,nd,itype)
    
    
     character(10) dtype,ddtype,filetype
     character(8) stationid
     character(50) file,file2
     real(4) rlon,rlat,stdno,rmeanb,rmeana,stdb,stda,tdatano
     real(4) tdatano2,xmedb,xmeda,smadb,smada,breakb1,breakb2,breake1,breake2
     real(4),dimension(43) :: plevel,plve1(43),plve2(43)
     

     data plevel/1000.,975.,950.,925.,900.,875.,850.,825.,800.,775.,750.,725.,700.,675.,650.,625.,600.,&
                 575.,550.,525.,500.,475.,450.,425.,400.,375.,350.,325.,300.,275.,250.,225.,&
                 200.,175.,150.,125.,100.,70.,50.,30.,20.,10.,5./




    Print *,' readwrite_list,dtype,filetype=',dtype,filetype,nd,itype

    do k=1,43
         if(k ==1) then
            plve1(k)=plevel(k)+200.0
            plve2(k)=plevel(k)-12.5
         else if( k <=37) then
            plve1(k)=plevel(k)+12.5
            plve2(k)=plevel(k)-12.5
         else if( k == 38) then
            plve1(k)=plevel(k)+17.5
            plve2(k)=plevel(k)-10.0
         else if( k ==  39 ) then
            plve1(k)=plevel(k)+10.0
            plve2(k)=plevel(k)-10.0
         else if( k ==  40 ) then
            plve1(k)=plevel(k)+10.0
            plve2(k)=plevel(k)-5.0
         else if( k ==41) then
            plve1(k)=plevel(k)+5.0
            plve2(k)=plevel(k)-5.0
         else if( k ==42) then
            plve1(k)=plevel(k)+5.0
            plve2(k)=plevel(k)-2.5
         else if ( k ==43) then
            plve1(k)=plevel(k)+2.5
            plve2(k)=0.0
         endif
      enddo


    ncount=0

    ddtype=dtype(1:1)
     ddtype=trim(ddtype)
     if( trim(filetype) == 'qlyrej' ) then
       file=trim(dtype)//'_'//trim(filetype)//'_list'
       file2=trim(dtype)//'_'//trim(filetype)//'_list_final'
    else if(trim(ddtype) /= 'u') then
       file=trim(dtype)//'_'//trim(filetype)//'_list'
       file2=trim(dtype)//'_'//trim(filetype)//'_list_final'
    else if(trim(ddtype) == 'u' .and. itype ==1) then
       file=trim(dtype)//'_'//trim(filetype)//'_dir_list'
       file2=trim(dtype)//'_'//trim(filetype)//'_dir_list_final'
     else if (trim(ddtype) == 'u' .and. itype ==2) then
       file=trim(dtype)//'_'//trim(filetype)//'_sp_list'
       file2=trim(dtype)//'_'//trim(filetype)//'_sp_list_final'
     endif
     open (21,file=file2,form='formatted')

     open (11,file=file,form='formatted')

     loopd: do

      read(11,100,IOSTAT=iflag) stationid,rlon,rlat,nlev
      if(iflag /=0 ) exit
       write(21,100) stationid,rlon,rlat,nlev
       ncount=ncount+1
       do k=1, nlev
          if( nd ==2) then
             read(11,120) klev,stdno,rmeanb,rmeana,stdb,stda,tdatano,tdatano2,iqcflg
             print *,klev,stdno,rmeanb,rmeana,stdb,stda,tdatano,tdatano2
             write(21,130) plve1(klev),plve2(klev),rmeanb,rmeana,stdb,stda,iqcflg
          else if(nd ==4) then
             read(11,140) klev,stdno,rmeanbw,rmeanaw,rmeanbs,rmeanas,stdbw,&
                          stdaw,stdbs,stdas,tdatano,tdatano2,iqcflg
             write(21,150) plve1(klev),plve2(klev),rmeanbw,rmeanaw,rmeanbs,rmeanas,stdbw,&
             stdaw,stdbs,stdas,iqcflg
          endif 
         
      
          read(11,160,IOSTAT=iflag) xmedb,xmeda,smadb,smada,breakb1,breakb2,breake1,breake2 
       enddo

     enddo   loopd

      

100 format(2x,a8,2f8.2,i8)
120 format(2x,i8,7f8.2,i3) 
130 format(2x,6f8.2,i3)
140 format(2x,i8,11f8.2,i3)
150 format(2x,10f8.2,i3)
160 format(2x,8f8.2)

    print *,' read_list2:ncount= ',ncount


!    do i=1,ncount
!       write(6,200) stdid(i),cid(i,1),cid(i,2),kklev(i)
!       do k=1,nk
!         if(rmean(i,k,1) >-900.0) then
!            write(6,210) k,rmean(i,k,1),rmean(i,k,2),&
!                     std(i,k,1),std(i,k,2),break(i,k,1),break(i,k,2),std_no(i,k)
!         endif
!      enddo
!   enddo
!
!200  format(a8,2f8.2,i8)
!210  format(i8,7f8.2)


       return
       end
 
