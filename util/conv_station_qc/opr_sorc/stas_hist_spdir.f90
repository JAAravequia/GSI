!   try to categorized the wind speed or direction o-b bias and standard deviation

     subroutine stas_hist_spdir(dtype,stdid,cid,rmean,std,nom_std,plev,nob,ndata,nk,rmiss)

     character(len=8),dimension(nob) ::  stdid
     real(4), dimension(nob,6) :: cid
     character(5)  dtype
     character(50) fileo1,fileo
     real(8),dimension(nob,nk,6) :: std,rmean
     real(8),dimension(nob,nk) :: nom_std
     real(4),dimension(nk) :: plev
     real(4),dimension(nk,51) :: rmeand_hist,stdd_hist,rmeans_hist,stds_hist 
     real(4),dimension(nk,50) :: rmeand_hist2,stdd_hist2,rmeans_hist2,stds_hist2,nstds_hist 
     integer,dimension(nk,4) :: ifc
     real(4),dimension(nk,4) :: crit
     

     real(4),dimension(51) :: hist1,hist2,hist3
     real factor1, factor2, factor3

     data factor1 /5.0/
     data factor2 /0.2/
     data factor3 /0.2/

      nhis=50

      rmeans_hist=0.0
      rmeand_hist=0.0
      stdd_hist=0.0
      stds_hist=0.0
      nstds_hist=0.0
      ifc=0
      crit=0.0

      do i =1,nhis+1
        hist1(i)=factor1*(I-1)
      enddo 

       do i =1,nhis+1
        hist2(i)=factor2*(I-1)
      enddo

      do i =1,nhis+1
        hist3(i)=factor3*(I-1)
      enddo



      print *,'stas_hist_spdir ',factor1,factor2,factor3


     do i=1,ndata
     do k=1,nk
        jj=0
       do j=1,nhis 
         if(rmean(i,k,1) >-200.0 .and. abs(rmean(i,k,1)) >hist1(nhis+1)) then
           jj=nhis
           exit
         else if(abs(rmean(i,k,1)) >=hist1(j) .and. abs(rmean(i,k,1)) <hist1(j+1)) then
           jj=j
           exit
         endif
       enddo
       if (jj >0) rmeand_hist(k,jj)=rmeand_hist(k,jj)+1.0  
        jj=0
       do j=1,nhis
         if(std(i,k,1) >hist1(nhis+1) ) then
            jj=nhis
            exit
         else if(std(i,k,1) >=hist1(j) .and. std(i,k,1) <hist1(j+1) ) then
           jj=j
           exit
         endif
       enddo
       if(jj >0) stdd_hist(k,jj)=stdd_hist(k,jj)+1.0
       jj=0
       do j=1,nhis
         if(rmean(i,k,3) >-200.0 .and. abs(rmean(i,k,3)) >hist2(nhis+1)) then
           jj=nhis
           exit
         else if(abs(rmean(i,k,3)) >=hist2(j) .and. abs(rmean(i,k,3)) <hist2(j+1)) then
           jj=j
           exit
         endif
       enddo
       if (jj >0) rmeans_hist(k,jj)=rmeans_hist(k,jj)+1.0
       jj=0
       do j=1,nhis
         if(std(i,k,3) >=hist2(nhis+1) ) then
            jj=nhis
            exit
         else if(std(i,k,3) >=hist2(j) .and. std(i,k,3) <hist2(j+1) ) then
           jj=j
           exit
         endif
       enddo
       if(jj >0) stds_hist(k,jj)=stds_hist(k,jj)+1.0
       jj=0
       do j=1,nhis
         if(nom_std(i,k) >=hist3(nhis+1) ) then
            jj=nhis
            exit 
         else if(nom_std(i,k) >=hist3(j) .and. nom_std(i,k) <hist3(j+1) ) then
           jj=j
           exit
         endif
       enddo
       if(jj >0) nstds_hist(k,jj)=nstds_hist(k,jj)+1.0
    enddo
    enddo
 
     
     do k=1,nk
      do i=1,nhis
        rmeand_hist(k,nhis+1)=rmeand_hist(k,nhis+1)+rmeand_hist(k,i)
        rmeans_hist(k,nhis+1)=rmeans_hist(k,nhis+1)+rmeans_hist(k,i)
        stds_hist(k,nhis+1)=stds_hist(k,nhis+1)+stds_hist(k,i)
        stdd_hist(k,nhis+1)=stdd_hist(k,nhis+1)+stdd_hist(k,i)
      enddo 
       do i=1,nhis
        if(rmeand_hist(k,nhis+1) >=1.0) & 
          rmeand_hist2(k,i)=rmeand_hist(k,i)/rmeand_hist(k,nhis+1)
        if(rmeans_hist(k,nhis+1) >=1.0) & 
          rmeans_hist2(k,i)=rmeans_hist(k,i)/rmeans_hist(k,nhis+1)
        if(stds_hist(k,nhis+1) >=1.0) & 
          stds_hist2(k,i)=stds_hist(k,i)/stds_hist(k,nhis+1)
        if(stdd_hist(k,nhis+1) >=1.0) & 
          stdd_hist2(k,i)=stdd_hist(k,i)/stdd_hist(k,nhis+1)
       enddo
    enddo 

       do k=1,nk
          facu=0.0
        do i=1,nhis
          facu=facu+rmeand_hist2(k,i)
          if(facu >=0.90 ) then
           ifc(k,1)=i
           exit
          endif
        enddo
     enddo

     do k=1,nk
          facu=0.0
        do i=1,nhis
          facu=facu+stdd_hist2(k,i)
          if(facu >=0.90 ) then
           ifc(k,2)=i
           exit
          endif
        enddo
     enddo

      do k=1,nk
          facu=0.0
        do i=1,nhis
          facu=facu+rmeans_hist2(k,i)
          if(facu >=0.90 ) then
           ifc(k,3)=i
           exit
          endif
        enddo
        enddo

        do k=1,nk
          facu=0.0
        do i=1,nhis
          facu=facu+stds_hist2(k,i)
          if(facu >=0.90 ) then
           ifc(k,4)=i
           exit
          endif
        enddo
     enddo


        do k=1,nk
        do j=1,2
          crit(k,j)= ifc(k,j)*factor1
        enddo
       do j=3,4
          crit(k,j)= ifc(k,j)*factor2
        enddo

        enddo

        call  creat_stas_hist_ctl(dtype,plev,nk,nhis,factor1,factor2,rmiss)

       fileo1=trim(dtype)//'_hist_90position'

        open(15,file=fileo1,form='formatted')
        write(15,550) dtype 
        do j=1,4
          write(15,500) (ifc(k,j),k=1,nk)
         enddo
         do j=1,4
          write(15,600) (crit(k,j),k=1,nk)
         enddo

550    format('dtype=',a8,' mean,std,direction,speed')
500    format(12i6)
600    format(12f6.2)



        fileo1=trim(dtype)//'_hist'

        open(20,file=fileo1,form='formatted')

         do k=1,nk
    
           write(20,*)'direction meand_hist ',k
           write(20,50) (rmeand_hist(k,i),i=1,nhis) 
           write(20,*)'direction mean_ratio_hist ',k  
           write(20,50) (rmeand_hist2(k,i),i=1,nhis) 
           write(20,*) 'direction stdd_hist ',k  
           write(20,50) (stdd_hist(k,i),i=1,nhis) 
           write(20,*)'direction std_ratio_hist ',k  
           write(20,50) (stdd_hist2(k,i),i=1,nhis) 
         enddo
50 format(9f7.2)

        close(20)
        do k=1,nk
        do j=1,nhis
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

        fileo1=trim(dtype)//'_hist_grads'

         open (30,file=fileo1,form='unformatted')

          do k=1,nk
            write(30) (rmeand_hist(k,j),j=1,nhis) 
          enddo
          do k=1,nk
            write(30) (rmeand_hist2(k,j),j=1,nhis) 
          enddo
          do k=1,nk
            write(30) (stdd_hist(k,j),j=1,nhis) 
          enddo
          do k=1,nk
            write(30) (stdd_hist2(k,j),j=1,nhis) 
          enddo
          do k=1,nk
            write(30) (rmeans_hist(k,j),j=1,nhis) 
          enddo
          do k=1,nk
            write(30) (rmeans_hist2(k,j),j=1,nhis) 
          enddo
          do k=1,nk
            write(30) (stds_hist(k,j),j=1,nhis) 
          enddo
          do k=1,nk
            write(30) (stds_hist2(k,j),j=1,nhis) 
          enddo

          close(30)

        fileo1=trim(dtype)//'_norsphist_grads'

        open(40,file=fileo1,form='unformatted')
          do k=1,nk
            write(40) (nstds_hist(k,j),j=1,nhis) 
          enddo

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
         do k=1,nk
           if(abs(rmean(i,k,1)) <300.0) then
              rrec=rrec+1.0
             if(abs(rmean(i,k,1)) >=10.0 .and. std(i,k,1) >=50.0)  rprb3=rprb3+1.0
             if( std(i,k,1) >=50.0)  rprb=rprb+1.0
             if( abs(rmean(i,k,1)) >=10.0)  rprb2=rprb2+1.0
           endif
        enddo
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
