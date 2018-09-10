module crite_constnt_reg

!   module:  crite_constnt_reg
!  this module contains all the criteria to reject potential problematic stations, black
!  list or bias correction list

    real,parameter ::  ps_c_rm     = 1.5  
    real,parameter ::  ps_c_std   = 1.3
    real,parameter ::  ps_c_smad   = 1.0


    real,parameter ::  q_c_rm     = 1.6
    real,parameter ::  q_c_std   = 1.3
    real,parameter ::  q_c_smad   = 0.8 
! marine surface pressure
    real,parameter ::  t4_c_rm     = 1.5
    real,parameter ::  t4_c_std   = 2.1
    real,parameter ::  t4_c_smad   = 1.3

! other surface temperature
    real,parameter ::  t3_c_rm     = 2.5
    real,parameter ::  t3_c_std   = 2.1
    real,parameter ::  t3_c_smad   = 1.3


    real,parameter ::  wd_c_rm    = 30.0  
    real,parameter ::  wd_c_std   = 70.0 
    real,parameter ::  ws_c_rm    = 2.5  
    real,parameter ::  ws_c_std   = 2.5 

    real ps_c_std2,q_c_std2,t_c_std2,ws_c_std2,wd_c_std2
    real,dimension(29) :: q2_c_rm,q2_c_std1
    real,dimension(43) :: t2_c_rm,t2_c_std1
    real,dimension(43) :: ws2_c_rm,ws2_c_std
    real,dimension(43) :: wd2_c_rm,wd2_c_std

   

    data q2_c_rm/1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,&
                 1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.6,1.7/
    data q2_c_std1/2.3,2.3,2.3,2.3,2.3,2.3,2.3,2.3,2.3,2.3,2.3,2.3,2.3,2.3,2.3,&
                   2.5,2.5,2.5,2.5,2.5,2.5,2.5,2.5,2.5,2.5,2.5,2.5,2.5,2.5/
!    data q2_c_std2/1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,&
!                   1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0/
!
    data t2_c_rm/2.0,2.0,2.0,1.5,1.3,1.3,1.3,1.3,1.3,1.3,1.3,1.3,1.3,1.3,1.3,1.3,&
                 1.3,1.3,1.3,1.3,1.3,1.3,1.3,1.3,1.3,1.3,1.3,1.3,1.3,1.3,1.3,1.3,1.3,1.3,1.3,1.3,1.3,&
                 1.3,1.3,1.3,1.5,1.8,3.8/
    data t2_c_std1/2.8,2.8,2.5,2.4,2.3,2.3,2.3,2.3,2.3,2.3,2.3,2.3,2.3,2.3,2.3,&
                   2.3,2.3,2.3,2.3,2.3,2.3,2.3,2.3,2.3,2.3,2.3,2.3,2.3,2.3,2.3,&
                   2.3,2.3,2.3,2.3,2.3,2.3,2.3,2.3,2.3,2.3,2.4,2.4,3.1/

!    data t2_c_std2/1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,&
!                   1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,&
!                   1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0/

     data wd2_c_rm/35.0,30.0,20.0,20.0,20.0,20.0,15.0,15.0,15.0,15.0,15.0,15.0,& 
                   15.0,15.0,15.0,15.0,15.0,15.0,10.0,10.0,10.0,10.0,10.0,10.0,&
                   5.0,10.0,10.0,10.0,5.0,10.0,5.0,10.0,5.0,10.0,10.0,10.0,&
                   10.0,10.0,10.0,10.0,10.0,15.0,20.0/
     data wd2_c_std/70.0,70.0,65.0,65.0,60.0,60.0,60.0,55.0,55.0,55.0,50.0,50.0,&
                   50.0,50.0,50.0,50.0,45.0,45.0,45.0,45.0,45.0,45.0,45.0,45.0,&
                   40.0,40.0,40.0,40.0,40.0,35.0,35.0,35.0,35.0,35.0,35.0,40.0,&
                   40.0,40.0,35.0,35.0,35.0,40.0,45.0/
     data ws2_c_rm/2.2,2.4,2.4,1.8,2.0,2.0,1.6,2.0,1.8,1.8,1.8,1.80,1.4,1.6,1.6,&
                   1.4,1.6,1.6,1.6,1.6,1.4,2.0,2.0,2.4,1.6,2.2,2.2,2.2,1.4,2.2,&
                   1.4,2.2,1.6,2.6,1.6,2.2,1.4,1.8,2.6,3.8,3.6,2.4,3.4/
     data ws2_c_std/2.4,3.2,3.2,3.0,3.4,3.4,3.0,3.4,3.2,3.4,3.4,3.4,3.2,3.6,3.4,&
                     3.6,3.4,3.8,3.6,3.8,3.4,3.8,4.0,4.2,3.6,4.4,4.6,4.8,4.2,5.0,&
                     4.4,4.8,4.4,4.8,4.4,4.4,3.6,3.0,2.8,2.8,3.0,4.0,5.2/ 

!contains
!  subroutine init_crite_constnt_derived
!
!
!        ps_c_std2=ps_c_rm/1.5
!        if(ps_c_std2 >1.0) ps_c_std2=1.0
!        q_c_std2=q_c_rm/1.5
!        if(q_c_std2 >1.0) q_c_std2=1.0
!        t_c_std2=t_c_rm/1.5
!        if(t_c_std2 >1.0) t_c_std2=1.0

!       do k=1,29
!         q2_c_std2(k)=q2_c_rm(k)/1.5
!         if(q2_c_std2(k) >1.0) q2_c_std2(k)=1.0
!       enddo
!       do k=1,43
!         t2_c_std2(k)=t2_c_rm(k)/1.5
!         if(t2_c_std2(k) >1.0) t2_c_std2(k)=1.0
!       enddo
!     
!           
!    return
!end subroutine init_crite_constnt_derived
                  



end module crite_constnt_reg

