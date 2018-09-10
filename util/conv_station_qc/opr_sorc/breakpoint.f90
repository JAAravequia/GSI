! the program is to detect the possible break point in the middile of data and 
! make the decision for the station whether it should be put into rejected list.


        subroutine breakpoint(n,i_break,x,xmed1,xmed2,xmad1,xmad2)


        real(8),intent(in),dimension(n) :: x
        
        real(8),dimension(n) :: x1,x2

        print *,'breakpoint:n=',n


        x2(1:i_break)=x(n-i_break+1:n)

        n2=i_break

        ii_break=2*i_break
       
        if(n >=ii_break) then
           n1=i_break 
           x1(1:n1)=x(1:n1) 
        else
           n1=n-i_break+1
           x1(1:n1)=x(1:n1)
        endif

         call med_a_mad(n1,x1,xmed1,xmad1)
         call med_a_mad(n2,x2,xmed2,xmad2)

         print *,'breakpoint:', n,n1,n2,xmed1,xmad1,xmed2,xmad2

          if(xmad1 >100.0) then
           write(6,*) n,n1,n2
           write(6,100) (x(i),i=1,n)
           write(6,100) (x1(i),i=1,n1)
          endif
          if(xmad2 >100.0) then
           write(6,*) n,n1,n2
           write(6,100) (x(i),i=1,n)
           write(6,100) (x2(i),i=1,n1)
          endif
        
100 format(10f7.1)        

         return
         end

