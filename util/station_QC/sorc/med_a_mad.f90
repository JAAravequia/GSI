! the subroutine estimate the median value and its scale

   subroutine med_a_mad(n,x,xmed,xmad)

   real(8),intent(in),dimension(n) :: x
   real(8),dimension(n) :: x1
   real(8) xmed,xmad

   x1(1:n)=x(1:n)

   call sort1(n,x1)
   
    n2=n/2

         if(2*n2 == n) then
            xmed=0.5*(x1(n2)+x1(n2+1))
          else
            xmed=x1(n2+1)
          endif

         do i=1,n
            x1(i)=abs(xmed-x1(i))
         enddo

           call sort1(n,x1)

           if(2*n2 == n) then
            xmad=0.5*(x1(n2)+x1(n2+1))
          else
            xmad=x1(n2+1)
          endif

          xmad=1.4826*xmad

           if(xmad >100) then
             write(6,90)  n
             write(6,100) (x(i),i=1,n) 
             write(6,100) (x1(i),i=1,n) 
           endif
90 format(1x,'med_a_mad',i5)            
100 format(10f8.1)

           return
           end

