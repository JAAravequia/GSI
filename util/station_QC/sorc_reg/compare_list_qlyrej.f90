!  the program read all three month data and put into one file for
!  statistics


    subroutine compare_list_qlyrej(mtm,dtype,tm,fileexist,nd,nd2,nk) 

    character(5) dtype,filetm1,filetype 
    character(1) ddtype 
    character(100) fileo,file1
 
    character(40),dimension(mtm) :: file,fileinput
    character(5),dimension(mtm) :: tm,filetm,filetm_o 
    character(5),dimension(mtm,mtm-1) :: filetm_other 
    logical,dimension(mtm) :: fileexist

    integer,dimension(5) :: nrecord
    
      character(len=8),dimension(200) :: stdid
      real(4),dimension(200,2) :: cid
      real(4),dimension(200,nk,2):: tdata_no
      real(4),dimension(200,nk,nd) :: std,rmean
      real(4),dimension(200,nk) :: std_no
      real(4),dimension(200,nk,nd2) :: break,xmed 
      integer,dimension(200,nk) :: iqcflg

      character(len=8),dimension(5,200) :: stdidout
      real(4),dimension(5,200,2) :: cidout
      real(4),dimension(5,200,nk,2) :: tdata_noout
      real(4),dimension(5,200,nk,nd) :: stdout,rmeanout
      real(4),dimension(5,200,nk) :: std_noout
      real(4),dimension(5,200,nk,nd2) :: breakout,xmedout 
       
      integer,dimension(200) :: klev,kklev
      integer,dimension(5,200,nk) :: iqcflgout 


!      tm(1)='00'
!      tm(2)='03'
!      tm(3)='06'
!      tm(4)='09'
!      tm(5)='12'

      if (mtm ==1) then
         return
         stop
      endif


      

      stdout=-999.0
      rmeanout=-999.0
      rmean=-999.0
      std=-999.0

       ndata=0    
       do i=1,mtm
          if( fileexist(i)) then
            ndata=ndata+1
            filetm(ndata)= tm(i) 
             if(i >1 .and. i <mtm) then
                do j=1,i-1 
                   filetm_other(ndata,j)= tm(j)
                enddo
                do j=i+1,mtm
                   filetm_other(ndata,j-1)= tm(j)
                enddo
             else if (i == 1) then
                do j=i+1,mtm
                   filetm_other(ndata,j-1)= tm(j)
                enddo
             else if( i ==mtm) then 
                do j=1,mtm-1
                   filetm_other(ndata,j)= tm(j)
                enddo
             endif
          endif
       enddo

       print *,filetm(1),filetm(2),filetm(3)
      print *, 'main_compare_list:',filetm_other(1,1),filetm_other(1,2) 
      print *, filetm_other(1,3) 

       if( ndata >mtm) then 
          print *, 'not valid ndata value,ndata=',ndata
           stop
       else if (ndata ==0) then
          print *, 'no file available, ndata=',ndata
         stop
       else 
          print *, 'ndata=',ndata
       endif
         
       do i=1,ndata
          filetm1=filetm(i) 
          do j=1,mtm-1
             filetm_o(j)= filetm_other(i,j)
          enddo 
         print *,'compare_list:filetm1=',filetm1,nd,nd2
         print *,'compare_list:filetm_o=',filetm_o(1),filetm_o(2),&
                  filetm_o(3),filetm_o(4)
            
          ddtype=dtype(1:1)
         if(ddtype == 'u') then
            file1=trim(dtype)//'_qlyrej_list_'//trim(tm(i))
            call read_listw(dtype,file1,200,ncount,nd,nd2,nk,stdid,cid,rmean,&
                         std,xmed,break,std_no,tdata_no,iqcflg,klev)
         else
            file1=trim(dtype)//'_qlyrej_list_'//trim(tm(i))
            call read_list2(dtype,file1,200,ncount,nd,nd2,nk,stdid,cid,rmean,&
                         std,xmed,break,std_no,tdata_no,iqcflg,klev)
         endif
         nrecord(i)=ncount
          stdidout(i,1:ncount)=stdid(1:ncount)
          cidout(i,1:ncount,1:2)=cid(1:ncount,1:2)
          rmeanout(i,1:ncount,1:nk,1:nd)=rmean(1:ncount,1:nk,1:nd)
          stdout(i,1:ncount,1:nk,1:nd)=std(1:ncount,1:nk,1:nd)
          xmedout(i,1:ncount,1:nk,1:nd2)=xmed(1:ncount,1:nk,1:nd2)
          breakout(i,1:ncount,1:nk,1:nd2)=break(1:ncount,1:nk,1:nd2)
          std_noout(i,1:ncount,1:nk)=std_no(1:ncount,1:nk)
          tdata_noout(i,1:ncount,1:nk,1:2)=tdata_no(1:ncount,1:nk,1:2)
          iqcflgout(i,1:ncount,1:nk)=iqcflg(1:ncount,1:nk)
          nrecord(i)=ncount
         print *,'compare_list:filetm1=',filetm1
         print *,nrecord(i)
        print *,'compare_list: allthelist'
         do j =1,ncount
           write(6,300) i,j,stdidout(i,j),cidout(i,j,1),cidout(i,j,2)
           do k=1,nk
             if(tdata_noout(i,j,k,1) >-900.0) then
               write(6,310) k,rmeanout(i,j,k,1),rmeanout(i,j,k,2),&
                            stdout(i,j,k,1),stdout(i,j,k,2),std_noout(i,j,k)
              write(6,320) (xmedout(i,j,k,jj),jj=1,4),(breakout(i,j,k,jj),jj=1,4)
            endif
         enddo
         enddo
      enddo

300  format(i8,1x,i8,1x,a8,2f8.2)
310  format(i8,5f8.2)
320  format(8f8.2)
  


     if(ndata ==1) then
        nstat=nrecord(1)
        print *, 'nstat=',nstat
        stdid(1:nstat)=stdidout(1,1:nstat)
        cid(1:nstat,1:2)=cidout(1,1:nstat,1:2)
        rmean(1:nstat,1:nk,1:nd)=rmeanout(1,1:nstat,1:nk,1:nd)
        std(1:nstat,1:nk,1:nd)=stdout(1,1:nstat,1:nk,1:nd)
        std_no(1:nstat,1:nk)=std_noout(1,1:nstat,1:nk)
        xmed(1:nstat,1:nk,1:nd2)=xmedout(1,1:nstat,1:nk,1:nd2)
        break(1:nstat,1:nk,1:nd2)=breakout(1,1:nstat,1:nk,1:nd2)
        tdata_no(1:nstat,1:nk,1:2)=tdata_noout(1,1:nstat,1:nk,1:2)       
        iqcflg(1:nstat,1:nk)=iqcflgout(1,1:nstat,1:nk)
     else if(ndata ==2) then
        nstat=nrecord(1)
        nstat2=nrecord(2)
        print *, 'nstat,nstat2=',nstat,nstat2
        call check_list_qlyrej(dtype,filetype,200,nd,nd2,nk,nstat2,nstat,2,1,&
             stdidout,cidout,rmeanout,stdout,xmedout,&
             breakout,std_noout,tdata_noout,iqcflgout,&
             stdid,cid,rmean,std,xmed,break,std_no,tdata_no,iqcflg,n)
        nstat=n
     else if( ndata ==3) then
        nstat=nrecord(1)
        nstat2=nrecord(2)
        nstat3=nrecord(3)
        print *, 'nstat,nstat2,nstat3=',nstat,nstat2,nstat3
        call check_list_qlyrej(dtype,filetype,200,nd,nd2,nk,nstat2,nstat,2,1,&
             stdidout,cidout,rmeanout,stdout,xmedout,&
             breakout,std_noout,tdata_noout,iqcflgout,&
             stdid,cid,rmean,std,xmed,break,std_no,tdata_no,iqcflg,n)
        nstat=n 

        

        stdidout(1,1:nstat)=stdid(1:nstat)
        cidout(1,1:nstat,1:2)=cid(1:nstat,1:2)
        rmeanout(1,1:nstat,1:nk,1:2)=rmean(1:nstat,1:nk,1:2)
        stdout(1,1:nstat,1:nk,1:2)=std(1:nstat,1:nk,1:2)
        std_noout(1,1:nstat,1:nk)=std_no(1:nstat,1:nk)
        xmedout(1,1:nstat,1:nk,1:4)=xmed(1:nstat,1:nk,1:4)
        breakout(1,1:nstat,1:nk,1:4)=break(1:nstat,1:nk,1:4)
        tdata_noout(1,1:nstat,1:nk,1:2)=tdata_no(1:nstat,1:nk,1:2)
        iqcflg(1:nstat,1:nk)=iqcflgout(1,1:nstat,1:nk)

        call check_list_qlyrej(dtype,filetype,200,nd,nd2,nk,nstat3,nstat,3,1,&
             stdidout,cidout,rmeanout,stdout,xmedout,&
             breakout,std_noout,tdata_noout,iqcflgout,&
             stdid,cid,rmean,std,xmed,break,std_no,tdata_no,iqcflg,n)
        nstat=n

     else if( ndata ==4) then
        nstat=nrecord(1)
        nstat2=nrecord(2)
        nstat3=nrecord(3)
        nstat4=nrecord(4)
        print *, 'nstat,nstat2,nstat3,nstat4=',nstat,nstat2,nstat3,nstat4        
        call check_list_qlyrej(dtype,filetype,200,nd,nd2,nk,nstat2,nstat,2,1,&
             stdidout,cidout,rmeanout,stdout,xmedout,&
             breakout,std_noout,tdata_noout,iqcflgout,&
             stdid,cid,rmean,std,xmed,break,std_no,tdata_no,iqcflg,n)
        nstat=n 

        stdidout(1,1:nstat)=stdid(1:nstat)
        cidout(1,1:nstat,1:2)=cid(1:nstat,1:2)
        rmeanout(1,1:nstat,1:nk,1:2)=rmean(1:nstat,1:nk,1:2)
        stdout(1,1:nstat,1:nk,1:2)=std(1:nstat,1:nk,1:2)
        std_noout(1,1:nstat,1:nk)=std_no(1:nstat,1:nk)
        xmedout(1,1:nstat,1:nk,1:4)=xmed(1:nstat,1:nk,1:4)
        breakout(1,1:nstat,1:nk,1:4)=break(1:nstat,1:nk,1:4)
        tdata_noout(1,1:nstat,1:nk,1:2)=tdata_no(1:nstat,1:nk,1:2)
        iqcflg(1:nstat,1:nk)=iqcflgout(1,1:nstat,1:nk)

        call check_list_qlyrej(dtype,filetype,200,nd,nd2,nk,nstat3,nstat,3,1,&
             stdidout,cidout,rmeanout,stdout,xmedout,&
             breakout,std_noout,tdata_noout,iqcflgout,&
             stdid,cid,rmean,std,xmed,break,std_no,tdata_no,iqcflg,n)
        nstat=n

        stdidout(1,1:nstat)=stdid(1:nstat)
        cidout(1,1:nstat,1:2)=cid(1:nstat,1:2)
        rmeanout(1,1:nstat,1:nk,1:2)=rmean(1:nstat,1:nk,1:2)
        stdout(1,1:nstat,1:nk,1:2)=std(1:nstat,1:nk,1:2)
        std_noout(1,1:nstat,1:nk)=std_no(1:nstat,1:nk)
        xmedout(1,1:nstat,1:nk,1:4)=xmed(1:nstat,1:nk,1:4)
        breakout(1,1:nstat,1:nk,1:4)=break(1:nstat,1:nk,1:4)
        tdata_noout(1,1:nstat,1:nk,1:2)=tdata_no(1:nstat,1:nk,1:2)
        iqcflg(1:nstat,1:nk)=iqcflgout(1,1:nstat,1:nk)

        call check_list_qlyrej(dtype,filetype,200,nd,nd2,nk,nstat4,nstat,4,1,&
             stdidout,cidout,rmeanout,stdout,xmedout,&
             breakout,std_noout,tdata_noout,iqcflgout,&
             stdid,cid,rmean,std,xmed,break,std_no,tdata_no,iqcflg,n)
        nstat=n
     else if(ndata ==5) then
        nstat=nrecord(1)
        nstat2=nrecord(2)
        nstat3=nrecord(3)
        nstat4=nrecord(4)
        nstat5=nrecord(5)
        print *, 'nstat,nstat2,nstat3,nstat4,nstat5=',nstat,nstat2,nstat3,nstat4,nstat5        
        call check_list_qlyrej(dtype,filetype,200,nd,nd2,nk,nstat2,nstat,2,1,&
             stdidout,cidout,rmeanout,stdout,xmedout,&
             breakout,std_noout,tdata_noout,iqcflgout,&
             stdid,cid,rmean,std,xmed,break,std_no,tdata_no,iqcflg,n)
        nstat=n 

        stdidout(1,1:nstat)=stdid(1:nstat)
        cidout(1,1:nstat,1:2)=cid(1:nstat,1:2)
        rmeanout(1,1:nstat,1:nk,1:2)=rmean(1:nstat,1:nk,1:2)
        stdout(1,1:nstat,1:nk,1:2)=std(1:nstat,1:nk,1:2)
        std_noout(1,1:nstat,1:nk)=std_no(1:nstat,1:nk)
        xmedout(1,1:nstat,1:nk,1:4)=xmed(1:nstat,1:nk,1:4)
        breakout(1,1:nstat,1:nk,1:4)=break(1:nstat,1:nk,1:4)
        tdata_noout(1,1:nstat,1:nk,1:2)=tdata_no(1:nstat,1:nk,1:2)
        iqcflg(1:nstat,1:nk)=iqcflgout(1,1:nstat,1:nk)

        call check_list_qlyrej(dtype,filetype,200,nd,nd2,nk,nstat3,nstat,3,1,&
             stdidout,cidout,rmeanout,stdout,xmedout,&
             breakout,std_noout,tdata_noout,iqcflgout,&
             stdid,cid,rmean,std,xmed,break,std_no,tdata_no,iqcflg,n)
        nstat=n

        stdidout(1,1:nstat)=stdid(1:nstat)
        cidout(1,1:nstat,1:2)=cid(1:nstat,1:2)
        rmeanout(1,1:nstat,1:nk,1:2)=rmean(1:nstat,1:nk,1:2)
        stdout(1,1:nstat,1:nk,1:2)=std(1:nstat,1:nk,1:2)
        std_noout(1,1:nstat,1:nk)=std_no(1:nstat,1:nk)
        xmedout(1,1:nstat,1:nk,1:4)=xmed(1:nstat,1:nk,1:4)
        breakout(1,1:nstat,1:nk,1:4)=break(1:nstat,1:nk,1:4)
        tdata_noout(1,1:nstat,1:nk,1:2)=tdata_no(1:nstat,1:nk,1:2)
        iqcflg(1:nstat,1:nk)=iqcflgout(1,1:nstat,1:nk)
        call check_list_qlyrej(dtype,filetype,200,nd,nd2,nk,nstat4,nstat,4,1,&
             stdidout,cidout,rmeanout,stdout,xmedout,&
             breakout,std_noout,tdata_noout,iqcflgout,&
             stdid,cid,rmean,std,xmed,break,std_no,tdata_no,iqcflg,n)
        nstat=n

        stdidout(1,1:nstat)=stdid(1:nstat)
        cidout(1,1:nstat,1:2)=cid(1:nstat,1:2)
        rmeanout(1,1:nstat,1:nk,1:2)=rmean(1:nstat,1:nk,1:2)
        stdout(1,1:nstat,1:nk,1:2)=std(1:nstat,1:nk,1:2)
        std_noout(1,1:nstat,1:nk)=std_no(1:nstat,1:nk)
        xmedout(1,1:nstat,1:nk,1:4)=xmed(1:nstat,1:nk,1:4)
        breakout(1,1:nstat,1:nk,1:4)=break(1:nstat,1:nk,1:4)
        tdata_noout(1,1:nstat,1:nk,1:2)=tdata_no(1:nstat,1:nk,1:2)
        iqcflg(1:nstat,1:nk)=iqcflgout(1,1:nstat,1:nk)
        call check_list_qlyrej(dtype,filetype,200,nd,nd2,nk,nstat5,nstat,5,1,&
             stdidout,cidout,rmeanout,stdout,xmedout,&
             breakout,std_noout,tdata_noout,&
             stdid,cid,rmean,std,xmed,break,std_no,tdata_no,n)
        nstat=n
     endif 

     klev=0
     do i=1,nstat
     do k=1,nk
        if(abs(rmean(i,k,1)) <900.0) then
           klev(i)=klev(i)+1
        endif
     enddo
       print *,'klev=',klev(i)
     enddo
         
         
     


     fileo=trim(dtype)//'_qlyrej_list'

     open(70,file=fileo,form='formatted')

      kklev=0

      do i=1,nstat
            do k=1,nk
               if(tdata_no(i,k,1) >=1.0 ) then
                 kklev(i)=kklev(i)+1
               endif
            enddo
      enddo
 

      do i=1,nstat
            write(70,100) stdid(i),cid(i,1),cid(i,2),kklev(i)
            do k=1,nk
               if(tdata_no(i,k,1) >=1.0 ) then
                 write(70,110) k,std_no(i,k),(rmean(i,k,jj),jj=1,nd),(std(i,k,jj),jj=1,nd),&
                         tdata_no(i,k,1),tdata_no(i,k,2),iqcflg(i,k)
                 write(70,120) (xmed(i,k,jj),jj=1,nd2),(break(i,k,jj),jj=1,nd2)
               endif
            enddo
      enddo

100 format(2x,a8,2f8.2,i8)
110 format(2x,i8,11f8.2,i3)
120 format(2x,8f8.2)


      

        return 
        end
