!   try to categorized the wind speed or direction o-b bias and standard deviation

     subroutine stas_hist(dtype,stdid,cid,rmean,std,nob,ndata,plev,nk,rmiss)

     character(len=8),dimension(nob) ::  stdid
     real(4), dimension(nob,6) :: cid
     character(5)  dtype
     character(50) fileo1,fileo
     real(8),dimension(nob,nk,5) :: std,rmean
     real(4),dimension(nk,61) :: rmean_hist,std_hist 
     real(4),dimension(nk,60) :: rmean_hist2,std_hist2 
     integer,dimension(nk,2) :: ifc
     real(4),dimension(nk,2) :: crit
     real(4),dimension(nk) :: plev
     real factor
     

    data factor/0.1/

     real(4),dimension(61) :: hist1

      rmean_hist=0.0
      std_hist=0.0
      nhis=60
      ifc=0
      crit=0.0
      
 
     print *,'stas_hist ','factor=',factor

      do i =1,nhis
        hist1(i)=factor*(I-1)
      enddo 
    
      hist1(61)=factor*nhis
     
     do i=1,ndata
     do k=1,nk
        jj=0
       do j=1,nhis 
         if(rmean(i,k,1) >-90.0 .and. abs(rmean(i,k,1)) >=hist1(nhis+1)) then
           jj=nhis
           exit
         else if(abs(rmean(i,k,1)) >=hist1(j) .and. abs(rmean(i,k,1)) <hist1(j+1)) then
           jj=j
           exit
         endif
       enddo
       if (jj >0) rmean_hist(k,jj)=rmean_hist(k,jj)+1.0  
        jj=0
       do j=1,nhis
         if(std(i,k,1) >=hist1(nhis+1) ) then
            jj=nhis
            exit
         else if(std(i,k,1) >=hist1(j) .and. std(i,k,1) <hist1(j+1) ) then
           jj=j
           exit
         endif
       enddo
       if(jj >0) std_hist(k,jj)=std_hist(k,jj)+1.0
    enddo
    enddo
 
     
     do k=1,nk
      do i=1,nhis
        rmean_hist(k,nhis+1)=rmean_hist(k,nhis+1)+rmean_hist(k,i)
        std_hist(k,nhis+1)=std_hist(k,nhis+1)+std_hist(k,i)
      enddo 
       do j=1,nhis
        if(rmean_hist(k,nhis+1) >0.0) & 
          rmean_hist2(k,j)=rmean_hist(k,j)/rmean_hist(k,nhis+1)
        if(std_hist(k,nhis+1) >0.0) & 
          std_hist2(k,j)=std_hist(k,j)/std_hist(k,nhis+1)
       enddo
    enddo 

     do k=1,nk
          facu=0.0
        do i=1,nhis
          facu=facu+rmean_hist2(k,i)
          if(facu >=0.90 ) then
           ifc(k,1)=i
           exit
          endif
        enddo
     enddo

        do k=1,nk
          facu=0.0
        do i=1,nhis
          facu=facu+std_hist2(k,i)
          if(facu >=0.90 ) then
           ifc(k,2)=i
           exit
          endif
        enddo
        enddo

        do k=1,nk
        do j=1,2
          crit(k,j)= ifc(k,j)*factor
        enddo
        enddo

       fileo1=trim(dtype)//'_hist_90position'

        open(15,file=fileo1,form='formatted') 

         write(15,550) dtype
         do j=1,2
          write(15,500) (ifc(k,j),k=1,nk)
         enddo
         do j=1,2
          write(15,600) (crit(k,j),k=1,nk)
         enddo

550    format('dtype=',a8,' mean,std')
500    format(12i6)
600    format(12f6.2)




        fileo1=trim(dtype)//'_hist'

        open(20,file=fileo1,form='formatted')

         do k=1,nk
    
           write(20,*)'mean ',k
           write(20,50) (rmean_hist(k,i),i=1,nhis) 
           write(20,*)'standard deviation ',k
           write(20,50) (std_hist(k,i),i=1,nhis) 
           write(20,*)'mean ratio ',k
           write(20,50) (rmean_hist2(k,i),i=1,nhis) 
           write(20,*)'std_ratio_hist ',k  
           write(20,50) (std_hist2(k,i),i=1,nhis) 
         enddo
50 format(9f10.2)

        close(20)
        do k=1,nk
        do j=1,nhis
          if(rmean_hist(k,j) == 0.0) rmean_hist(k,j)=-999.0
          if(rmean_hist2(k,j) == 0.0) rmean_hist2(k,j)=-999.0
          if(std_hist(k,j) == 0.0) std_hist(k,j)=-999.0
          if(std_hist2(k,j) == 0.0) std_hist2(k,j)=-999.0
        enddo
        enddo

        fileo1=trim(dtype)//'_hist_grads'

         open (30,file=fileo1,form='unformatted')

          do k=1,nk
            write(30) (rmean_hist(k,j),j=1,nhis) 
          enddo
          do k=1, nk
            write(30) (rmean_hist2(k,j),j=1,nhis) 
          enddo
          do k=1,nk
            write(30) (std_hist(k,j),j=1,nhis) 
          enddo
          do k=1,nk
            write(30) (std_hist2(k,j),j=1,nhis) 
          enddo

          close(30)

         call  creat_stas_hist_ctl(dtype,plev,nk,nhis,factor,factor2,rmiss) 

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
           if(abs(rmean(i,k,1)) <80.0) then
              rrec=rrec+1.0
             if(abs(rmean(i,k,1)) >=crit(k,1) .and. std(i,k,1) >=crit(k,2))  rprb3=rprb3+1.0
             if( std(i,k,1) >=crit(k,2))  rprb=rprb+1.0
             if( abs(rmean(i,k,1)) >=crit(k,1))  rprb2=rprb2+1.0
           endif
        enddo
          if(rrec >0.0) rate=rprb/rrec 
           nprob=nprob+1
           if( rrec >0.0) then
              write(10,100) stdid(i),cid(i,1),cid(i,2),cid(i,3),cid(i,4),cid(i,5),&
                         rate,rprb,rprb2,rprb3,rrec
           endif
!           if(abs(cid(i,2)) <16.00 .and. rrec >0.0) then
!              rntro=rntro+1.0
!              write(50,100) stdid(i),cid(i,1),cid(i,2),cid(i,3),cid(i,4),cid(i,5),&
!                          rate,rprb,rprb2,rprb3,rrec
!              if(rate >=0.5 ) suptro=suptro+1.0
!           endif
      enddo

100 format(a8,2f7.2,8f6.1)
 
        write(50,*) rntro,suptro
          
close(10)
close(50)
          
       return
       end
