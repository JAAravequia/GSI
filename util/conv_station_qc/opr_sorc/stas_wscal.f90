!   to calculate statistics for each wind scale

    subroutine stas_wscal(dtype,stdid,cid,nk,nob,nsta,nt,tdata)

    character(len=8),dimension(nob) ::  stdid
     real(4), dimension(nob,6) :: cid
     character(5)  dtype
     character(50) fileo1,fileo

     real(4),dimension(nob,nk,14,nt) :: tdata 
     real(4),dimension(12) :: wscale
     real(8),dimension(nob,12,2) :: rmean,std
     real(4),dimension(nob,12) :: rno
     integer,dimension(12,4) :: ifc

     real(4),dimension(36) :: hist1,hist2,hist3
     real(4),dimension(12,37) :: rmeand_hist,stdd_hist,rmeans_hist,stds_hist
     real(4),dimension(12,36) :: rmeand_hist2,stdd_hist2,rmeans_hist2,stds_hist2

     data wscale/0.3,1.5,3.4,5.4,7.9,10.7,13.8,17.1,20.7,24.4,32.6,100.0/


   print *,'tas_wscal:',nk,nob,nsta,nt

    do i =1,36
      hist1(i)=5.0*I
      hist2(i)=0.5*I
      hist3(i)=0.2*I 
    enddo


    rmean=0.0
    std=0.0
    rno=0.0

    print *,'begin tas_spdir_wscal '
     

     do i=1,nsta
     do k=1,nk
     do it=1,nt
       jj=0
       do j=1,12
         if(tdata(i,k,14,it) >-300.0 .and. tdata(i,k,14,it) <= wscale(j)) then
            jj=j
            exit
         endif
       enddo
        if( jj >0) then
           rno(i,j)=rno(i,j)+1.0
           rmean(i,j,1)=rmean(i,j,1)+tdata(i,k,9,it)
           rmean(i,j,2)=rmean(i,j,2)+tdata(i,k,11,it)
           std(i,j,1)=std(i,j,1)+tdata(i,k,9,it)*tdata(i,k,9,it)
           std(i,j,2)=std(i,j,2)+tdata(i,k,11,it)*tdata(i,k,11,it)
        endif
     enddo
     enddo
     enddo

      print  *,rno(1,1),rno(1,2),rno(1,3)


     do i=1,nsta
     do j=1,12
       if (rno(i,j) >1.0) then 
         do k=1,2
           std(i,j,k)=sqrt((std(i,j,k)-rmean(i,j,k)*rmean(i,j,k)/rno(i,j))/rno(i,j))
           rmean(i,j,k)=rmean(i,j,k)/rno(i,j)
         enddo
       else if(rno(i,j) == 1.0) then
        do k=1,2
          std(i,j,k)=0.0
          rmean(i,j,k)=rmean(i,j,k)
        enddo
       else
        do k=1,2
         std(i,j,k)=-999.0
         rmean(i,j,k)=-999.0
        enddo
       endif
    enddo
    enddo      

      print  *,rmean(1,1,1),rmean(1,2,1),rmean(1,3,1)
      print  *,std(1,1,1),std(1,2,1),std(1,3,1)
     do i=1,nsta
     do k=1,12
        jj=0
       do j=1,36
         if(rmean(i,k,1) >-299.0 .and. abs(rmean(i,k,1)) <=hist1(j)) then
           jj=j
           print *,jj
           exit
         endif
       enddo
       if (jj >0) rmeand_hist(k,jj)=rmeand_hist(k,jj)+1.0
        jj=0
       do j=1,36
         if(std(i,k,1) >0.0 .and. std(i,k,1) <=hist1(j)) then
           jj=j
           exit
         endif
       enddo
       if(jj >0) stdd_hist(k,jj)=stdd_hist(k,jj)+1.0
       jj=0
       do j=1,36
         if(rmean(i,k,2) >-299.0 .and. abs(rmean(i,k,2)) <=hist2(j)) then
           jj=j
           exit
         endif
       enddo
       if (jj >0) rmeans_hist(k,jj)=rmeans_hist(k,jj)+1.0
       do j=1,36
         if(std(i,k,2) >0.0 .and. std(i,k,2) <=hist2(j)) then
           jj=j
         exit
         endif
       enddo
       if(jj >0) stds_hist(k,jj)=stds_hist(k,jj)+1.0
    enddo
    enddo
 

     do k=1,12
      do i=1,36
        rmeand_hist(k,37)=rmeand_hist(k,37)+rmeand_hist(k,i)
        rmeans_hist(k,37)=rmeans_hist(k,37)+rmeans_hist(k,i)
        stds_hist(k,37)=stds_hist(k,37)+stds_hist(k,i)
        stdd_hist(k,37)=stdd_hist(k,37)+stdd_hist(k,i)
      enddo
       do i=1,36
        if(rmeand_hist(k,37) >0.0) &
          rmeand_hist2(k,i)=rmeand_hist(k,i)/rmeand_hist(k,37)
        if(rmeans_hist(k,37) >0.0) &
          rmeans_hist2(k,i)=rmeans_hist(k,i)/rmeans_hist(k,37)
        if(stds_hist(k,37) >0.0) &
          stds_hist2(k,i)=stds_hist(k,i)/stds_hist(k,37)
        if(stdd_hist(k,37) >0.0) &
          stdd_hist2(k,i)=stdd_hist(k,i)/stdd_hist(k,37)
       enddo
    enddo

        do k=1,12
          facu=0.0
        do i=1,36
          facu=facu+rmeand_hist2(k,i)
          if(facu >=0.95 ) then
           ifc(k,1)=i 
           exit
          endif
        enddo
        enddo

        do k=1,12
          facu=0.0
        do i=1,36
          facu=facu+rmeans_hist2(k,i)
          if(facu >=0.95 ) then
           ifc(k,2)=i
           exit
          endif
        enddo
        enddo

        do k=1,12
          facu=0.0
        do i=1,36
          facu=facu+stdd_hist2(k,i)
          if(facu >=0.95 ) then
           ifc(k,3)=i
           exit
          endif
        enddo
        enddo

        do k=1,12
          facu=0.0
        do i=1,36
          facu=facu+stds_hist2(k,i)
          if(facu >=0.95 ) then
           ifc(k,4)=i
           exit
          endif
        enddo
        enddo




fileo1=trim(dtype)//'_spscal_hist'

        open(20,file=fileo1,form='formatted')

         do k=1,12
           write(20,*)'direction meand_hist'
           write(20,50) (rmeand_hist(k,i),i=1,36)
           write(20,*)'direction mean_ratio_hist'
           write(20,50) (rmeand_hist2(k,i),i=1,36)
           write(20,*) 'direction stdd_hist'
           write(20,50) (stdd_hist(k,i),i=1,36)
           write(20,*)'direction std_ratio_hist'
           write(20,50) (stdd_hist2(k,i),i=1,36)
         enddo
50 format(9f7.2)

         close(20)

        do k=1,12
        do j=1,36
          if(rmeand_hist(k,j) == 0.0) rmeand_hist(k,j)=-999.0
          if(rmeand_hist2(k,j) == 0.0) rmeand_hist2(k,j)=-999.0
          if(stdd_hist(k,j) == 0.0) stdd_hist(k,j)=-999.0
          if(stdd_hist2(k,j) == 0.0) stdd_hist2(k,j)=-999.0
          if(rmeans_hist(k,j) == 0.0) rmeans_hist(k,j)=-999.0
          if(rmeans_hist2(k,j) == 0.0) rmeans_hist2(k,j)=-999.0
          if(stds_hist(k,j) == 0.0) stds_hist(k,j)=-999.0
          if(stds_hist2(k,j) == 0.0) stds_hist2(k,j)=-999.0
        enddo
        enddo

        fileo1=trim(dtype)//'_spscal_hist_grads'

         open (30,file=fileo1,form='unformatted')

          do k=1,12
            write(30) (rmeand_hist(k,j),j=1,36)
          enddo
          do k=1,12
            write(30) (rmeand_hist2(k,j),j=1,36)
          enddo
          do k=1,12
            write(30) (stdd_hist(k,j),j=1,36)
          enddo
          do k=1,12
            write(30) (stdd_hist2(k,j),j=1,36)
          enddo
          do k=1,12
            write(30) (rmeans_hist(k,j),j=1,36)
          enddo
          do k=1,12
            write(30) (rmeans_hist2(k,j),j=1,36)
          enddo
          do k=1,12
            write(30) (stds_hist(k,j),j=1,36)
          enddo
          do k=1,12
            write(30) (stds_hist2(k,j),j=1,36)
          enddo

          close(30)


          fileo1=trim(dtype)//'_spscal_grads'
          open(40,file=fileo1,form='unformatted')

            do i=1,12
              write(40) rmean(1:nsta,i,1)
            enddo
            do i=1,12
              write(40) rmean(1:nsta,i,2)
            enddo
            do i=1,12
              write(40) std(1:nsta,i,1)
            enddo
            do i=1,12
              write(40) std(1:nsta,i,2)
            enddo
           do i=1,12
              write(40) rno(1:nsta,i)
            enddo


       close(40)

 fileo1=trim(dtype)//'_wscal_95position'
       open(50,file=fileo1,form='formatted')

        do j=1,4
        write(50,500) (ifc(k,j),k=1,12)
        enddo
500    format(12i6)
 fileo1=trim(dtype)//'_wscal_stationlist'
       open(10,file=fileo1,form='formatted')


         do i=1,nsta
          rrec=0.0
          rprb=0.0
          rprb2=0.0
          rprb3=0.0
          rate=0.0
          nprob=0
         do k=2,12
           if(abs(rmean(i,k,1)) <800.0) then
              rrec=rrec+1.0
             if(k == 2 .and. abs(rmean(i,k,1)) >=50.0 .and. &
                std(i,k,1) >=100.0)  rprb3=rprb3+1.0
             if(k == 3 .and. abs(rmean(i,k,1)) >=25.0 .and. &
                std(i,k,1) >=85.0)  rprb3=rprb3+1.0
             if(k == 4 .and. abs(rmean(i,k,1)) >=15.0 .and. &
                std(i,k,1) >=65.0)  rprb3=rprb3+1.0
             if(k == 5 .and. abs(rmean(i,k,1)) >=10.0 .and. &
                std(i,k,1) >=50.0)  rprb3=rprb3+1.0
             if(k == 6 .and. abs(rmean(i,k,1)) >=5.0 .and. &
                std(i,k,1) >=40.0)  rprb3=rprb3+1.0
             if(k == 7 .and. abs(rmean(i,k,1)) >=5.0 .and. &
                std(i,k,1) >=30.0)  rprb3=rprb3+1.0
             if(k == 8 .and. abs(rmean(i,k,1)) >=5.0 .and. &
                std(i,k,1) >=25.0)  rprb3=rprb3+1.0
             if(k == 9 .and. abs(rmean(i,k,1)) >=5.0 .and. &
                std(i,k,1) >=20.0)  rprb3=rprb3+1.0
             if(k > 9 .and. abs(rmean(i,k,1)) >=5.0 .and. &
                std(i,k,1) >=10.0)  rprb3=rprb3+1.0
           endif
        enddo
           if( rrec >0.0) then
              write(10,100) stdid(i),cid(i,1),cid(i,2),cid(i,3),cid(i,4),cid(i,5),&
                         rprb3,rrec
           endif
      enddo

100 format(a8,7f8.2)


       


         print *,'end of stas_spdir_wscal'

         return
         end
