!  the program read all three month data and put into one file for
!  statistics


    subroutine compare_list_sfc_qlyrej(nfile,dtype,tm,fileexist,nd,nd2) 

    character(5) dtype,filetm1,filetype 
    character(1) ddtype
    character(100) fileo,file1
 
    character(40),dimension(nfile) :: file,fileinput
    character(5),dimension(nfile) :: tm,filetm,filetm_o 
    character(5),dimension(nfile,nfile-1) :: filetm_other 
    logical,dimension(nfile) :: fileexist

    integer,dimension(5) :: nrecord
    
      character(len=8),dimension(2000) :: stdid
      real(4),dimension(2000,2) :: cid,tdata_no
      real(4),dimension(2000,nd) :: std,rmean
      real(4),dimension(2000) :: std_no
      real(4),dimension(2000,nd2) :: break,xmed 
      integer,dimension(7000) :: iqcflg

      character(len=8),dimension(5,2000) :: stdidout
      real(4),dimension(5,2000,2) :: cidout,tdata_noout
      real(4),dimension(5,2000,nd) :: stdout,rmeanout
      real(4),dimension(5,2000) :: std_noout
      real(4),dimension(5,2000,nd2) :: breakout,xmedout 
      integer,dimension(5,7000) :: iqcflgout



!      tm(1)='00'
!      tm(2)='03'
!      tm(3)='06'
!      tm(4)='09'
!      tm(5)='12'
      
        if (nfile ==1) then
          return
          stop
       endif


       ddtype =dtype(1:1)

       ndata=0    
       do i=1,nfile
          if( fileexist(i)) then
            ndata=ndata+1
            filetm(ndata)= tm(i) 
             if(i >1 .and. i <nfile) then
                do j=1,i-1 
                   filetm_other(ndata,j)= tm(j)
                enddo
                do j=i+1,nfile
                   filetm_other(ndata,j-1)= tm(j)
                enddo
             else if (i == 1) then
                do j=i+1,nfile
                   filetm_other(ndata,j-1)= tm(j)
                enddo
             else if( i ==nfile) then 
                do j=1,nfile-1
                   filetm_other(ndata,j)= tm(j)
                enddo
             endif
          endif
       enddo

       print *,filetm(1),filetm(2),filetm(3),filetm(4)
      print *, 'main_compare_list_sfc:',filetm_other(1,1),filetm_other(1,2) 
      print *, filetm_other(1,3) 

       if( ndata >nfile) then 
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
          do j=1,nfile-1
             filetm_o(j)= filetm_other(i,j)
          enddo 
         print *,'main_compare_list_sfc:filetm1=',filetm1
         print *,'main_compare_list_sfc:filetm_o=',filetm_o(1),filetm_o(2),&
                  filetm_o(3),filetm_o(4)
          ddtype=dtype(1:1)
         if(ddtype == 'u') then
            file1=trim(dtype)//'_qlyrej_list_'//trim(tm(i))
            call read_listw_sfc(dtype,file1,2000,ncount,nd,nd2,stdid,cid,rmean,&
                         std,xmed,break,std_no,tdata_no,iqcflg)
         else
            file1=trim(dtype)//'_qlyrej_list_'//trim(tm(i))
            call read_list2_sfc(dtype,file1,2000,ncount,nd,nd2,stdid,cid,rmean,&
                         std,xmed,break,std_no,tdata_no,iqcflg)
         endif
         nrecord(i)=ncount
          stdidout(i,1:ncount)=stdid(1:ncount)
          cidout(i,1:ncount,1:2)=cid(1:ncount,1:2)
          rmeanout(i,1:ncount,1:nd)=rmean(1:ncount,1:nd)
          stdout(i,1:ncount,1:nd)=std(1:ncount,1:nd)
          xmedout(i,1:ncount,1:nd2)=xmed(1:ncount,1:nd2)
          breakout(i,1:ncount,1:nd2)=break(1:ncount,1:nd2)
          std_noout(i,1:ncount)=std_no(1:ncount)
          tdata_noout(i,1:ncount,1:2)=tdata_no(1:ncount,1:2)
          iqcflgout(i,1:ncount)=iqcflg(1:ncount) 
          nrecord(i)=ncount
         print *,'main_compare_list_sfc:filetm1=',filetm1
         print *,nrecord(i)
!         do k =1,ncount
!           write(6,300) stdidout(i,k),cidout(i,k,1),cidout(i,k,2),(rmeanout(i,k,jj),jj=1,2),&
!                        std_noout(i,k),(tdata_noout(i,k,jj),jj=1,2) 
!           write(6,310) (xmedout(i,k,jj),jj=1,4),(breakout(i,k,jj),jj=1,4)
!         enddo
      enddo

300  format(a8,7f8.2)
310  format(8f8.2)
  


     if(ndata ==1) then
        nstat=nrecord(1)
        print *, 'nstat=',nstat
        stdid(1:nstat)=stdidout(1,1:nstat)
        cid(1:nstat,1:2)=cidout(1,1:nstat,1:2)
        rmean(1:nstat,1:nd)=rmeanout(1,1:nstat,1:nd)
        std(1:nstat,1:nd)=stdout(1,1:nstat,1:nd)
        std_no(1:nstat)=std_noout(1,1:nstat)
        xmed(1:nstat,1:nd2)=xmedout(1,1:nstat,1:nd2)
        break(1:nstat,1:nd2)=breakout(1,1:nstat,1:nd2)
        tdata_no(1:nstat,1:2)=tdata_noout(1,1:nstat,1:2)       
        iqcflg(1:nstat)=iqcflgout(1,1:nstat)
     else if(ndata ==2) then
        nstat=nrecord(1)
        nstat2=nrecord(2)
        print *, 'nstat,nstat2=',nstat,nstat2
        call check_list_sfc_qlyrej(dtype,filetype,2000,nd,nd2,nstat2,nstat,2,1,&
             stdidout,cidout,rmeanout,stdout,xmedout,&
             breakout,std_noout,tdata_noout,iqcflgout,&
             stdid,cid,rmean,std,xmed,break,std_no,tdata_no,iqcflg,n)
        nstat=n
     else if( ndata ==3) then
        nstat=nrecord(1)
        nstat2=nrecord(2)
        nstat3=nrecord(3)
        print *, 'nstat,nstat2=',nstat,nstat3
        call check_list_sfc_qlyrej(dtype,filetype,2000,nd,nd2,nstat2,nstat,2,1,&
             stdidout,cidout,rmeanout,stdout,xmedout,&
             breakout,std_noout,tdata_noout,iqcflgout,&
             stdid,cid,rmean,std,xmed,break,std_no,tdata_no,iqcflg,n)
        nstat=n 

        stdidout(1,1:nstat)=stdid(1:nstat)
        cidout(1,1:nstat,1:2)=cid(1:nstat,1:2)
        rmeanout(1,1:nstat,1:nd)=rmean(1:nstat,1:nd)
        stdout(1,1:nstat,1:nd)=std(1:nstat,1:nd)
        xmedout(1,1:nstat,1:nd2)=xmed(1:nstat,1:nd2)
        breakout(1,1:nstat,1:nd2)=break(1:nstat,1:nd2)
        std_noout(1,1:nstat)=std_no(1:nstat)
        tdata_noout(1,1:nstat,1:2)=tdata_no(1:nstat,1:2)
        iqcflgout(1,1:nstat)=iqcflg(1:nstat)

        call check_list_sfc_qlyrej(dtype,filetype,2000,nd,nd2,nstat3,nstat,3,1,&
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
        call check_list_sfc_qlyrej(dtype,filetype,2000,nd,nd2,nstat2,nstat,2,1,&
             stdidout,cidout,rmeanout,stdout,xmedout,&
             breakout,std_noout,tdata_noout,iqcflgout,&
             stdid,cid,rmean,std,xmed,break,std_no,tdata_no,iqcflg,n)
        nstat=n 

        stdidout(1,1:nstat)=stdid(1:nstat)
        cidout(1,1:nstat,1:2)=cid(1:nstat,1:2)
        rmeanout(1,1:nstat,1:nd)=rmean(1:nstat,1:nd)
        stdout(1,1:nstat,1:nd)=std(1:nstat,1:nd)
        xmedout(1,1:nstat,1:nd2)=xmed(1:nstat,1:nd2)
        breakout(1,1:nstat,1:nd2)=break(1:nstat,1:nd2)
        std_noout(1,1:nstat)=std_no(1:nstat)
        tdata_noout(1,1:nstat,1:2)=tdata_no(1:nstat,1:2)
        iqcflgout(1,1:nstat)=iqcflg(1:nstat)

        call check_list_sfc_qlyrej(dtype,filetype,2000,nd,nd2,nstat3,nstat,3,1,&
             stdidout,cidout,rmeanout,stdout,xmedout,&
             breakout,std_noout,tdata_noout,iqcflgout,&
             stdid,cid,rmean,std,xmed,break,std_no,tdata_no,iqcflg,n)
        nstat=n

        stdidout(1,1:nstat)=stdid(1:nstat)
        cidout(1,1:nstat,1:2)=cid(1:nstat,1:2)
        rmeanout(1,1:nstat,1:nd)=rmean(1:nstat,1:nd)
        stdout(1,1:nstat,1:nd)=std(1:nstat,1:nd)
        xmedout(1,1:nstat,1:nd2)=xmed(1:nstat,1:nd2)
        breakout(1,1:nstat,1:nd2)=break(1:nstat,1:nd2)
        std_noout(1,1:nstat)=std_no(1:nstat)
        tdata_noout(1,1:nstat,1:2)=tdata_no(1:nstat,1:2)
        iqcflgout(1,1:nstat)=iqcflg(1:nstat)
        call check_list_sfc_qlyrej(dtype,filetype,2000,nd,nd2,nstat4,nstat,4,1,&
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
        call check_list_sfc_qlyrej(dtype,filetype,2000,nd,nd2,nstat2,nstat,2,1,&
             stdidout,cidout,rmeanout,stdout,xmedout,&
             breakout,std_noout,tdata_noout,iqcflgout,&
             stdid,cid,rmean,std,xmed,break,std_no,tdata_no,iqcflg,n)
        nstat=n 

        stdidout(1,1:nstat)=stdid(1:nstat)
        cidout(1,1:nstat,1:2)=cid(1:nstat,1:2)
        rmeanout(1,1:nstat,1:nd)=rmean(1:nstat,1:nd)
        stdout(1,1:nstat,1:nd)=std(1:nstat,1:nd)
        xmedout(1,1:nstat,1:nd2)=xmed(1:nstat,1:nd2)
        breakout(1,1:nstat,1:nd2)=break(1:nstat,1:nd2)
        std_noout(1,1:nstat)=std_no(1:nstat)
        tdata_noout(1,1:nstat,1:2)=tdata_no(1:nstat,1:2)
        iqcflgout(1,1:nstat)=iqcflg(1:nstat)
        call check_list_sfc_qlyrej(dtype,filetype,2000,nd,nd2,nstat3,nstat,3,1,&
             stdidout,cidout,rmeanout,stdout,xmedout,&
             breakout,std_noout,tdata_noout,iqcflgout,&
             stdid,cid,rmean,std,xmed,break,std_no,tdata_no,iqcflg,n)
        nstat=n

        stdidout(1,1:nstat)=stdid(1:nstat)
        cidout(1,1:nstat,1:2)=cid(1:nstat,1:2)
        rmeanout(1,1:nstat,1:nd)=rmean(1:nstat,1:nd)
        stdout(1,1:nstat,1:nd)=std(1:nstat,1:nd)
        xmedout(1,1:nstat,1:nd2)=xmed(1:nstat,1:nd2)
        breakout(1,1:nstat,1:nd2)=break(1:nstat,1:nd2)
        std_noout(1,1:nstat)=std_no(1:nstat)
        tdata_noout(1,1:nstat,1:2)=tdata_no(1:nstat,1:2)
        iqcflgout(1,1:nstat)=iqcflg(1:nstat)
        call check_list_sfc_qlyrej(dtype,filetype,2000,nd,nd2,nstat4,nstat,4,1,&
             stdidout,cidout,rmeanout,stdout,xmedout,&
             breakout,std_noout,tdata_noout,iqcflgout,&
             stdid,cid,rmean,std,xmed,break,std_no,tdata_no,iqcflg,n)
        nstat=n

        stdidout(1,1:nstat)=stdid(1:nstat)
        cidout(1,1:nstat,1:2)=cid(1:nstat,1:2)
        rmeanout(1,1:nstat,1:nd)=rmean(1:nstat,1:nd)
        stdout(1,1:nstat,1:nd)=std(1:nstat,1:nd)
        xmedout(1,1:nstat,1:nd2)=xmed(1:nstat,1:nd2)
        breakout(1,1:nstat,1:nd2)=break(1:nstat,1:nd2)
        std_noout(1,1:nstat)=std_no(1:nstat)
        tdata_noout(1,1:nstat,1:2)=tdata_no(1:nstat,1:2)
        iqcflgout(1,1:nstat)=iqcflg(1:nstat)
        call check_list_sfc_qlyrej(dtype,filetype,2000,nd,nd2,nstat5,nstat,5,1,&
             stdidout,cidout,rmeanout,stdout,xmedout,&
             breakout,std_noout,tdata_noout,iqcflgout,&
             stdid,cid,rmean,std,xmed,break,std_no,tdata_no,iqcflg,n)
        nstat=n
     endif 


     fileo=trim(dtype)//'_qlyrej_list'

     open(70,file=fileo,form='formatted')


        do i=1,nstat
           write(70,100) stdid(i),cid(i,1),cid(i,2),(rmean(i,j),j=1,nd),&
                 (std(i,j),j=1,nd),iqcflg(i)
           write(70,110) tdata_no(i,1),tdata_no(i,2),(xmed(i,j),j=1,nd2)
           write(70,120) std_no(i),(break(i,j),j=1,nd2)
        enddo

100 format(2x,a8,10f8.2,i3)
110 format(2x,6f8.2)
120 format(2x,5f8.2)


      

        return 
        end
