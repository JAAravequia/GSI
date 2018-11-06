!  the program read all three month data and put into one file for
!  statistics

!  ntm:  how many tm files;nd the data items in rmean and sd files, nd2: data
!  items in xmed and xmed, nothing to do with tm


    subroutine compare_list_sfc(ntm,dtype,tm,fileexist,filetype,nd,nd2) 

    character(5) dtype,filetype 
    character(2) filetm1 
    character(1) ddtype
    character(50) fileo
 
    character(40),dimension(ntm) :: file,fileinput
    character(2),dimension(ntm) :: filetm,filetm_o 
    character(2),dimension(ntm) :: tm 
    character(2),dimension(ntm,ntm-1) :: filetm_other 
    logical,dimension(ntm) :: fileexist

    integer,dimension(5) :: nrecord
    
      character(len=8),dimension(7000) :: stdid
      real(4),dimension(7000,2) :: cid,tdata_no
      real(4),dimension(7000,nd) :: std,rmean
      real(4),dimension(7000) :: std_no
      real(4),dimension(7000,nd2) :: break,xmed 
      integer,dimension(7000) :: iqcflg

      character(len=8),dimension(5,7000) :: stdidout
      real(4),dimension(5,7000,2) :: cidout,tdata_noout
      real(4),dimension(5,7000,nd) :: stdout,rmeanout
      real(4),dimension(5,7000) :: std_noout
      real(4),dimension(5,7000,nd2) :: breakout,xmedout 
      integer,dimension(5,7000) :: iqcflgout



!      tm(1)='03'
!      tm(2)='06'
!      tm(3)='09'
!      tm(4)='12'
       filetm_other='     '
       filetm_o='     '
       filetm='   '
        print *,'compare_list_sfc:ntm=',ntm
       ddtype =dtype(1:1)
       if (ntm ==1) then
          return
          stop
       endif

       ndata=0    
       do i=1,ntm
          if( fileexist(i)) then
            ndata=ndata+1
            filetm(ndata)= tm(i) 
            print *,'compare_list_sfc:filetm(ndata)=',ndata,filetm(ndata)
             if(i >1 .and. i <ntm) then
                do j=1,i-1 
                       filetm_other(ndata,j)= tm(j)
                       print *,'compare_list_sfc-1:filetm_other(ndata,j)=',ndata,j,i,filetm_other(ndata,j),tm(j)
                enddo
                do j=i+1,ntm
                       filetm_other(ndata,j-1)= tm(j)
                       print *,'compare_list_sfc-2:filetm_other(ndata,j-1)=',ndata,j,filetm_other(ndata,j-1),tm(j)
                enddo
             else if (i == 1) then
                do j=i+1,ntm
                      filetm_other(ndata,j-1)= tm(j)
                       print *,'compare_list_sfc-3:filetm_other(ndata,j-1)=',j,filetm_other(ndata,j-1),tm(j)
                enddo
             else if( i ==ntm) then 
                do j=1,ntm-1
                      filetm_other(ndata,j)= tm(j)
                       print *,'compare_list_sfc-4:filetm_other(ndata,j)=',j,filetm_other(ndata,j),tm(j)
                enddo
             endif
          endif
       enddo

       print *,'compare_list_sfc,filetm(1:2):',filetm(1),filetm(2),filetm(3)
!      print *, 'compare_list_sfc:filetm_other=',filetm_other(1,1),filetm_other(1,2),filetm_other(1,3) 
!      print *, 'compare_list_sfc:',filetm_other(2,1),filetm_other(2,2),filetm_other(2,3) 
!      print *, 'compare_list_sfc:',filetm_other(3,1),filetm_other(3,2),filetm_other(3,3) 
!      print *, 'compare_list_sfc:',filetm_other(4,1),filetm_other(4,2),filetm_other(4,3) 

       if( ndata >ntm) then 
          print *, 'not valid ndata value,ndata=',ndata
           stop
       else if (ndata ==0) then
          print *, 'no bias or rejection files for this type , ndata=',ndata
         stop
       else 
          print *, 'ndata=',ndata
       endif
       do i=1,ndata
          filetm1=filetm(i) 
          do j=1, ntm-1
             filetm_o(j)= filetm_other(i,j)
          enddo 
         print *,'compare_list_sfc:filetm1=',filetm1
         print *,'compare_list_sfc:filetm_o=',filetm_o(1),filetm_o(2),filetm_o(2),filetm_o(3)

          if( ntm ==2) then
             call check_data_sfc_ttm2(ntm,dtype,filetype,filetm1,filetm_o,7000,nd,nd2,stdid,cid,rmean,&
                   std,std_no,tdata_no,xmed,break,iqcflg,ncount)
          else if (ntm ==3) then
             call check_data_sfc_ttm3(ntm,dtype,filetype,filetm1,filetm_o,7000,nd,nd2,stdid,cid,rmean,&
                   std,std_no,tdata_no,xmed,break,iqcflg,ncount)
          else if(ntm ==4) then
             call check_data_sfc_ttm4(ntm,dtype,filetype,filetm1,filetm_o,7000,nd,nd2,stdid,cid,rmean,&
                   std,std_no,tdata_no,xmed,break,iqcflg,ncount)
         endif
 
              
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
        call check_list_sfc(dtype,filetype,7000,nd,nd2,nstat2,nstat,2,1,&
             stdidout,cidout,rmeanout,stdout,xmedout,&
             breakout,std_noout,tdata_noout,iqcflgout,&
             stdid,cid,rmean,std,xmed,break,std_no,tdata_no,iqcflg,n)
        nstat=n
     else if( ndata ==3) then
        nstat=nrecord(1)
        nstat2=nrecord(2)
        nstat3=nrecord(3)
        print *, 'nstat,nstat2=',nstat,nstat3
        call check_list_sfc(dtype,filetype,7000,nd,nd2,nstat2,nstat,2,1,&
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

        call check_list_sfc(dtype,filetype,7000,nd,nd2,nstat3,nstat,3,1,&
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
        call check_list_sfc(dtype,filetype,7000,nd,nd2,nstat2,nstat,2,1,&
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

        print *, 'end of check_list_sfc :qcflg(1)=',iqcflg(1)        
        call check_list_sfc(dtype,filetype,7000,nd,nd2,nstat3,nstat,3,1,&
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
        call check_list_sfc(dtype,filetype,7000,nd,nd2,nstat4,nstat,4,1,&
             stdidout,cidout,rmeanout,stdout,xmedout,&
             breakout,std_noout,tdata_noout,iqcflgout,&
             stdid,cid,rmean,std,xmed,break,std_no,tdata_no,iqcflg,n)
        nstat=n
        print *, 'end of check_list_sfc :qcflg(2)=',iqcflg(2)        
     else if(ndata ==5) then
        nstat=nrecord(1)
        nstat2=nrecord(2)
        nstat3=nrecord(3)
        nstat4=nrecord(4)
        nstat5=nrecord(5)
        print *, 'nstat,nstat2,nstat3,nstat4,nstat5=',nstat,nstat2,nstat3,nstat4,nstat5        
        call check_list_sfc(dtype,filetype,7000,nd,nd2,nstat2,nstat,2,1,&
             stdidout,cidout,rmeanout,stdout,xmedout,&
             breakout,std_noout,tdata_noout,iqcflgout,&
             stdid,cid,rmean,std,xmed,break,std_no,tdata_no,iqcflg,n)
        nstat=n 
        print *, 'end of check_list_sfc :qcflg(2)=',iqcflg(2)        

        stdidout(1,1:nstat)=stdid(1:nstat)
        cidout(1,1:nstat,1:2)=cid(1:nstat,1:2)
        rmeanout(1,1:nstat,1:nd)=rmean(1:nstat,1:nd)
        stdout(1,1:nstat,1:nd)=std(1:nstat,1:nd)
        xmedout(1,1:nstat,1:nd2)=xmed(1:nstat,1:nd2)
        breakout(1,1:nstat,1:nd2)=break(1:nstat,1:nd2)
        std_noout(1,1:nstat)=std_no(1:nstat)
        tdata_noout(1,1:nstat,1:2)=tdata_no(1:nstat,1:2)
        iqcflgout(1,1:nstat)=iqcflg(1:nstat)
        call check_list_sfc(dtype,filetype,7000,nd,nd2,nstat3,nstat,3,1,&
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
        call check_list_sfc(dtype,filetype,7000,nd,nd2,nstat4,nstat,4,1,&
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

        print *, 'end of check_list_sfc :qcflg(2)=',iqcflg(3)        

        call check_list_sfc(dtype,filetype,7000,nd,nd2,nstat5,nstat,5,1,&
             stdidout,cidout,rmeanout,stdout,xmedout,&
             breakout,std_noout,tdata_noout,iqcflgout,&
             stdid,cid,rmean,std,xmed,break,std_no,tdata_no,iqcflg,n)
        nstat=n
     endif 

     if (trim(ddtype) == 'u') then
        fileo=trim(dtype)//'_'//trim(filetype)//'_dir_list'
     else
        fileo=trim(dtype)//'_'//trim(filetype)//'_list'
     endif

     open(70,file=fileo,form='formatted')


        do i=1,nstat
           if(ddtype == 'u' ) then
              write(70,100) stdid(i),cid(i,1),cid(i,2),(rmean(i,j),j=1,nd),&
                 (std(i,j),j=1,nd),iqcflg(i)
          else
             write(70,150) stdid(i),cid(i,1),cid(i,2),(rmean(i,j),j=1,nd),&
                 (std(i,j),j=1,nd),iqcflg(i)
          endif
              write(70,110) tdata_no(i,1),tdata_no(i,2),(xmed(i,j),j=1,nd2)
              write(70,120) std_no(i),(break(i,j),j=1,nd2)
        enddo

100 format(2x,a8,10f8.2,i3)
110 format(2x,6f8.2)
120 format(2x,5f8.2)
150 format(2x,a8,6f8.2,i3)


      

        return 
        end
