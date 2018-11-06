!   try to categorized the wind speed or direction o-b bias and standard deviation

     subroutine stas_hist_spdir_sfc(dtype,stdid,cid,rmean,std,nom_std,nob,ndata,rmiss)

     character(len=8),dimension(nob) ::  stdid
     real(4), dimension(nob,6) :: cid
     character(5)  dtype
     character(50) fileo1,fileo
     real(4),dimension(nob,6) :: std,rmean
     real(8),dimension(nob) :: nom_std
     real(4),dimension(51) :: rmeand_hist,stdd_hist,rmeans_hist,stds_hist 
     real(4),dimension(50) :: rmeand_hist2,stdd_hist2,rmeans_hist2,stds_hist2,nstds_hist 
     integer,dimension(4) :: ifc
     real(4),dimension(4) :: crit
     

     real(4),dimension(51) :: hist1,hist2,hist3
     real factor1, factor2, factor3

     data factor1 /5.0/
     data factor2 /0.2/
     data factor3 /0.2/
     data plev /1000.0/

      nk=1
      nhis=50

      ifc=0
      crit=0.0
      rmeans_hist=0.0
      rmeand_hist=0.0
      stdd_hist=0.0
      stds_hist=0.0
      nstds_hist=0.0

      do i =1,nhis+1
        hist1(i)=factor1*(I-1)
!       print *,'hist1=',hist1(i),i
      enddo 

       do i =1,nhis+1
        hist2(i)=factor2*(I-1)
      enddo

      do i =1,nhis+1
        hist3(i)=factor3*(I-1)
      enddo

     do i=1,ndata
        jj=0
       do j=1,nhis 
         if(rmean(i,1) >-300.0 .and. abs(rmean(i,1)) >hist1(nhis+1)) then
           jj=nhis
!           print *,'rmean(i,1),jj=',rmean(i,1),jj,hist1(nhis+1)
           exit
         else if(abs(rmean(i,1)) >=hist1(j) .and. abs(rmean(i,1)) <hist1(j+1)) then
           jj=j
         print *,'rmean(i,1),jj=',rmean(i,1),jj,hist1(j)
           exit
         endif
       enddo
       if (jj >0) rmeand_hist(jj)=rmeand_hist(jj)+1.0  
        jj=0
       do j=1,nhis
         if(std(i,1) >hist1(nhis+1) ) then
            jj=nhis
           print *,'std(i,1),jj=',std(i,1),jj
            exit
         else if(std(i,1) >=hist1(j) .and. std(i,1) <hist1(j+1) ) then
           jj=j
           print *,'std(i,1),jj=',std(i,1),jj
           exit
         endif
       enddo
       if(jj >0) stdd_hist(jj)=stdd_hist(jj)+1.0
       jj=0
       do j=1,nhis
         if(rmean(i,3) >-300.0 .and. abs(rmean(i,3)) >hist2(nhis+1)) then
           jj=nhis
           exit
         else if(abs(rmean(i,3)) >=hist2(j) .and. abs(rmean(i,3)) <hist2(j+1)) then
           jj=j
           exit
         endif
       enddo
       if (jj >0) rmeans_hist(jj)=rmeans_hist(jj)+1.0
       jj=0
       do j=1,nhis
         if(std(i,3) >=hist2(nhis+1) ) then
            jj=nhis
            exit
         else if(std(i,3) >=hist2(j) .and. std(i,3) <hist2(j+1) ) then
           jj=j
           exit
         endif
       enddo
       if(jj >0) stds_hist(jj)=stds_hist(jj)+1.0
       jj=0
       do j=1,nhis
         if(nom_std(i) >=hist3(nhis+1) ) then
            jj=nhis
            exit
         else if(nom_std(i) >=hist3(j) .and. nom_std(i) <hist3(j+1) ) then
           jj=j
           exit
         endif
       enddo
       if(jj >0) nstds_hist(jj)=nstds_hist(jj)+1.0
    enddo
     
      do i=1,nhis
        rmeand_hist(nhis+1)=rmeand_hist(nhis+1)+rmeand_hist(i)
        rmeans_hist(nhis+1)=rmeans_hist(nhis+1)+rmeans_hist(i)
        stds_hist(nhis+1)=stds_hist(nhis+1)+stds_hist(i)
        stdd_hist(nhis+1)=stdd_hist(nhis+1)+stdd_hist(i)
      enddo 
       do i=1,nhis
        if(rmeand_hist(nhis+1) >0.0) & 
          rmeand_hist2(i)=rmeand_hist(i)/rmeand_hist(nhis+1)
        if(rmeans_hist(nhis+1) >0.0) & 
          rmeans_hist2(i)=rmeans_hist(i)/rmeans_hist(nhis+1)
        if(stds_hist(nhis+1) >0.0) & 
          stds_hist2(i)=stds_hist(i)/stds_hist(nhis+1)
        if(stdd_hist(nhis+1) >0.0) & 
          stdd_hist2(i)=stdd_hist(i)/stdd_hist(nhis+1)
       enddo

          facu=0.0
        do i=1,nhis
          facu=facu+rmeand_hist2(i)
          if(facu >=0.90 ) then
           ifc(1)=i
           exit
          endif
        enddo

          facu=0.0
        do i=1,nhis
          facu=facu+stdd_hist2(i)
          if(facu >=0.90 ) then
           ifc(2)=i
           exit
          endif
        enddo

          facu=0.0
        do i=1,nhis
          facu=facu+rmeans_hist2(i)
          if(facu >=0.90 ) then
           ifc(3)=i
           exit
          endif
        enddo

          facu=0.0
        do i=1,nhis
          facu=facu+stds_hist2(i)
          if(facu >=0.90 ) then
           ifc(4)=i
           exit
          endif
        enddo


        do j=1,2
          crit(j)= ifc(j)*factor1
        enddo
       do j=3,4
          crit(j)= ifc(j)*factor2
        enddo

       call creat_stas_hist_ctl(dtype,plev,nk,nhis,factor1,factor2,rmiss)


       fileo1=trim(dtype)//'_hist_90position'

        open(15,file=fileo1,form='formatted')
          write(15,550) dtype 
          write(15,500) (ifc(j),j=1,4)
          write(15,600) (crit(j),j=1,4)

550    format('dtype=',a8,'  mean,std,direction,speed')
500    format(4i6)
600    format(4f6.2)



        fileo1=trim(dtype)//'_hist'

        open(20,file=fileo1,form='formatted')

           write(20,*)'direction meand_hist'
           write(20,50) (rmeand_hist(i),i=1,nhis) 
           write(20,*)'direction mean_ratio_hist'  
           write(20,50) (rmeand_hist2(i),i=1,nhis) 
           write(20,*) 'direction stdd_hist'  
           write(20,50) (stdd_hist(i),i=1,nhis) 
           write(20,*)'direction std_ratio_hist'  
           write(20,50) (stdd_hist2(i),i=1,nhis) 
50 format(9f7.2)

        close(20)
        do j=1,nhis
          if(rmeand_hist(j) == 0.0) rmeand_hist(j)=-999.0
          if(rmeand_hist2(j) == 0.0) rmeand_hist2(j)=-999.0
          if(stdd_hist(j) == 0.0) stdd_hist(j)=-999.0
          if(stdd_hist2(j) == 0.0) stdd_hist2(j)=-999.0
          if(rmeans_hist(j) == 0.0) rmeans_hist(j)=-999.0
          if(rmeans_hist2(j) == 0.0) rmeans_hist2(j)=-999.0
          if(stds_hist(j) == 0.0) stds_hist(j)=-999.0
          if(stds_hist2(j) == 0.0) stds_hist2(j)=-999.0
        enddo

        fileo1=trim(dtype)//'_hist_grads'

         open (30,file=fileo1,form='unformatted')

            write(30) (rmeand_hist(j),j=1,nhis) 
            write(30) (rmeand_hist2(j),j=1,nhis) 
            write(30) (stdd_hist(j),j=1,nhis) 
            write(30) (stdd_hist2(j),j=1,nhis) 
            write(30) (rmeans_hist(j),j=1,nhis) 
            write(30) (rmeans_hist2(j),j=1,nhis) 
            write(30) (stds_hist(j),j=1,nhis) 
            write(30) (stds_hist2(j),j=1,nhis) 

          close(30)

        fileo1=trim(dtype)//'_norsphist_grads'

        open(40,file=fileo1,form='unformatted')
            write(40) (nstds_hist(j),j=1,nhis) 

          close(40)
          

        fileo1=trim(dtype)//'stationlist'
        fileo=trim(dtype)//'_topical_stationlist'
        open(10,file=fileo1,form='formatted')
        open(50,file=fileo,form='formatted')

       rntro=0.0
       suptro=0.0
       

       do i=1,ndata
          rrec=0.0
          rprb=0.0
          rprb2=0.0
          rprb3=0.0
          rate=0.0
          nprob=0
           if(abs(rmean(i,1)) <200.0) then
              rrec=rrec+1.0
             if(abs(rmean(i,1)) >=crit(1) .and. std(i,1) >=crit(2))  rprb3=rprb3+1.0
             if( std(i,1) >=crit(2))  rprb=rprb+1.0
             if( abs(rmean(i,1)) >=crit(1))  rprb2=rprb2+1.0
           endif
          if(rrec >0.0) rate=rprb/rrec 
           nprob=nprob+1
           if( rrec >0.0) then
              write(10,100) stdid(i),cid(i,1),cid(i,2),cid(i,3),cid(i,4),cid(i,5),&
                         rate,rprb,rprb2,rprb3,rrec
           endif
           if(abs(cid(i,2)) <16.00 .and. rrec >0.0) then
              rntro=rntro+1.0
              write(50,100) stdid(i),cid(i,1),cid(i,2),cid(i,3),cid(i,4),cid(i,5),&
                          rate,rprb,rprb2,rprb3,rrec
              if(rate >=0.5 ) suptro=suptro+1.0
           endif
      enddo

100 format(a8,2f7.2,8f6.1)
 
        write(50,*) rntro,suptro
          
close(10)
close(50)
          
       return
       end
