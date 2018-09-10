        subroutine sort1(n,ra)      !  from the numerical recipes

         real(8),dimension(n) :: ra
         real(8) rra

          L=n/2+1
          ir=N
10       continue
          if(L >1 ) then
             L=L-1
             rra=ra(L)
          else
             rra=ra(ir)
             ra(ir)=ra(1)
             ir=ir-1
             if (ir == 1) then
               ra(1)=rra
               return
             endif
           endif
              i=L
              j=L+L
20            if (j <= ir) then
                if(j <ir) then
                   if(ra(j) <ra(j+1)) j=j+1
                endif
                if(rra < ra(j)) then
                  ra(i)=ra(j)
                  i=J
                  j=j+1
                else
                  j=ir+1
                endif
                go to 20
              endif
               ra(i)=rra
               go to 10
               end
