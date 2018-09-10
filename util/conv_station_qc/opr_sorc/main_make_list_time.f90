!  the program read all three month data and put into one file for
!  statistics


    character(5) dtype,tm 
    integer itype
    integer ntype
    character(1) ddtype


    namelist/input/dtype,itype


     read(5,input)
     write(6,input)
     write(6,*)' '

       print *,'call select begin'

        ddtype=dtype(1:1)
        ddtype=trim(ddtype)

        print *,'ddtype ',ddtype,itype

       if(itype == 0  .and. trim(ddtype) == 'u') then
          call make_list_time_sfc(dtype,4)
       else if(itype == 0  .and. trim(ddtype) /='u') then
          call make_list_time_sfc(dtype,2)
       else if (itype == 1  .and. trim(ddtype) == 'u') then
          call make_list_time(dtype,4,43)
       else if( itype == 1  .and. trim(ddtype) == 'q') then
          call make_list_time(dtype,2,29)
       else if( itype == 1  .and. trim(ddtype) == 't') then
          call make_list_time(dtype,2,43)
       endif



        stop
        end
