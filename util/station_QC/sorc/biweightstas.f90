!  the program is to calculate the mean and standard deviation with less impact with outliers
!   biweight mean and standard deviation( J.R. Lanzante:  Resistant, robust and non-paraemtric 
!    techniques for the analysis of climate data: Theory and examples, including applications 
!    to historical radiosonde station data.  International Journal of Climatology, vol 16, 1996.
!
!
!   formular:  Ui=(Xi-M)/(c*MAD)   c:6~9  for the gaussian case c=6(9) censors values more 
!                                  than 4(6) standard deviation from the mean.  c=7.5 more 
!                                  than 5 standard  deviation  
!           Xbi=M+{&[(Xi-M)*(1-Ui**Ui)*(1-Ui**Ui)]/&[(1-Ui*Ui)*(1-Ui*Ui)]}
!           Sbi=sqrt{n*& (Xi-M)*(Xi-M)*(1-Ui**2)**4}/|&(1-Ui**2)(1-5*Ui**2)|
!
!
!        M: median value, MAD dedian absolute deviation
!
!
        subroutine biweight(n,x,xmed,xb,sb,smad)

        real(8),intent(in),dimension(n) :: x
        real(8),dimension(n) :: x1
        real(8) c,xmad,xb2,sb2,u
        real(8),intent(out) :: xmed,xb,sb,smad
     

        data c/7.5/

        x1=x 

         call sort1(n,x1)
      
          n2=n/2
          if(2*n2 == n) then
            xmed=0.5*(x1(n2)+x1(n2+1))
          else
            xmed=x1(n2+1)
          endif

!          print *,'xmed=',xmed
       
           do i=1,n
            x1(i)=abs(xmed-x1(i))
           enddo

           call sort1(n,x1)

           if(2*n2 == n) then
            xmad=0.5*(x1(n2)+x1(n2+1))
          else
            xmad=x1(n2+1)
          endif


          xb=0.0
          xb2=0.0
          sb=0.0
          sb2=0.0
          do i=1,n 
           u=(x(i)-xmed)/(c*xmad)
           xb=xb+(x(i)-xmed)*(1.0-u*u)*(1.0-u*u)
           xb2=xb2+(1.0-u*u)*(1.0-u*u) 
           sb=sb+(x(i)-xmed)*(x(i)-xmed)*(1.0-u*u)**4
           sb2=sb2+(1.0-u*u)*(1.0-5*u*u)
          enddo

          if( xb2 /=0.0) then
            xb=xmed+xb/xb2
          else
            xb=xmed
          endif

         sb2=abs(sb2)
         if (sb2 /=0.0) then
          sb=sqrt(n*sb)/sb2
         else
          sb=-999.0
         endif 
          
          smad=1.483*xmad

!         print *, 'biweight:',xmed,xmad,smad,xb,sb
 
         return
         end
   

