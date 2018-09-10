!   try to categorized the wind speed or direction o-b bias and standard deviation

     subroutine stas_hist_sfc(dtype,stdid,cid,rmean,std,nob,ndata,rmiss)

     character(len=8),dimension(nob) ::  stdid
     real(4), dimension(nob,6) :: cid
     character(5)  dtype
     character(50) fileo1,fileo
     real(4),dimension(nob,5) :: std,rmean
     real(4),dimension(61) :: rmean_hist,std_hist 
     real(4),dimension(60) :: rmean_hist2,std_hist2 
     integer,dimension(2) :: ifc
     real(4),dimension(2) :: crit
     

     real(4),dimension(61) :: hist1
     real(4),dimension(1) :: plev
     real factor

     data plev/1000.0/
     data factor/0.1/

      nk=1
 
      print *,'start histogram stas_hist_sfc:',nob,ndata
      print *,(stdid(i),i=1,10)
      print *,(cid(i,1),i=1,10)
      print *,(rmean(i,1),i=1,10)
      print *,(std(i,1),i=1,10)

      rmean_hist=0.0
      std_hist=0.0
      nhis=60
      ifc=0
      crit=0.0

      do i =1,nhis
        hist1(i)=factor*(I-1)
      enddo 

  
 
      hist1(61)=factor*nhis
      

     do i=1,ndata
        jj=0
       do j=1,nhis 
         if(rmean(i,1) >-90.0 .and. abs(rmean(i,1)) >=hist1(nhis+1)) then
           jj=nhis
           exit
         else if(abs(rmean(i,1)) >=hist1(j) .and. abs(rmean(i,1)) <hist1(j+1)) then
           jj=j
           exit
         endif
       enddo
       if (jj >0) rmean_hist(jj)=rmean_hist(jj)+1.0  
       jj=0
       do j=1,nhis
         if(std(i,1) >=hist1(nhis+1) ) then
            jj=nhis
            exit
         else if(std(i,1) >=hist1(j) .and. std(i,1) <hist1(j+1) ) then
           jj=j
           exit
         endif
       enddo
       if(jj >0) std_hist(jj)=std_hist(jj)+1.0
    enddo
 
     
      do i=1,nhis
        rmean_hist(nhis+1)=rmean_hist(nhis+1)+rmean_hist(i)
        std_hist(nhis+1)=std_hist(nhis+1)+std_hist(i)
      enddo 
       do i=1,nhis
        if(rmean_hist(nhis+1) >0.0) & 
          rmean_hist2(i)=rmean_hist(i)/rmean_hist(nhis+1)
        if(std_hist(nhis+1) >0.0) & 
          std_hist2(i)=std_hist(i)/std_hist(nhis+1)
       enddo

          facu=0.0
        do i=1,nhis
          facu=facu+rmean_hist2(i)
          if(facu >=0.90 ) then
           ifc(1)=i
           exit
          endif
        enddo

          facu=0.0
        do i=1,nhis
          facu=facu+std_hist2(i)
          if(facu >=0.90 ) then
           ifc(2)=i
           exit
          endif
        enddo

        do j=1,2
          crit(j)= ifc(j)*factor
        enddo

       call creat_stas_hist_ctl(dtype,plev,nk,nhis,factor,factor2,rmiss)

       fileo1=trim(dtype)//'_hist_90position'

        open(15,file=fileo1,form='formatted') 

          write(15,550) dtype 
          write(15,500) ifc(1),ifc(2)
          write(15,600) crit(1),crit(2)

550    format('dtype=',a8,'  mean,std')
500    format(2i6)
600    format(2f6.2)



        fileo1=trim(dtype)//'_hist'

        open(20,file=fileo1,form='formatted')

           write(20,*)'mean'
           write(20,50) (rmean_hist(i),i=1,nhis) 
           write(20,*)'standard deviation'
           write(20,50) (std_hist(i),i=1,nhis) 
           write(20,*)'mean ratio'
           write(20,50) (rmean_hist2(i),i=1,nhis) 
           write(20,*)'std_ratio_hist'  
           write(20,50) (std_hist2(i),i=1,nhis) 
50 format(9f7.2)

        close(20)
        do j=1,nhis
          if(rmean_hist(j) == 0.0) rmean_hist(j)=-999.0
          if(rmean_hist2(j) == 0.0) rmean_hist2(j)=-999.0
          if(std_hist(j) == 0.0) std_hist(j)=-999.0
          if(std_hist2(j) == 0.0) std_hist2(j)=-999.0
        enddo

        fileo1=trim(dtype)//'_hist_grads'

         open (30,file=fileo1,form='unformatted')

            write(30) (rmean_hist(j),j=1,nhis) 
            write(30) (rmean_hist2(j),j=1,nhis) 
            write(30) (std_hist(j),j=1,nhis) 
            write(30) (std_hist2(j),j=1,nhis) 

          close(30)

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
              rrec=rrec+1.0                  !  count of station which has observations
             if(abs(rmean(i,1)) >=crit(1) .and. std(i,1) >=crit(2))  rprb3=rprb3+1.0
             if( std(i,1) >=crit(2))  rprb=rprb+1.0
             if( abs(rmean(i,1)) >=crit(1))  rprb2=rprb2+1.0
           endif
        enddo
          if(rrec >0.0) rate=rprb/rrec 
           nprob=nprob+1
           if( rrec >0.0) then
              write(10,100) stdid(i),cid(i,1),cid(i,2),cid(i,3),cid(i,4),cid(i,5),&
                         rate,rprb,rprb2,rprb3,rrec
           endif

100 format(a8,2f7.2,8f6.1)
 
        write(50,*) rntro,suptro
          
close(10)
close(50)
          
       return
       end
