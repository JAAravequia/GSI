!  the program read all three month data and put into one file for
!  statistics


    character(5) dtype 
    integer itype
    integer ntype
    character*1 ddtype


    namelist/input/dtype,itype


     read(5,input)
     write(6,input)
     write(6,*)' '

        ddtype=dtype(1:1)
        ddtype=trim(ddtype)

       print *, 'ddtype ',ddtype,itype
        if(itype == 0 .and. ddtype == 'u') then
          n=14
          call read_data_sfc(dtype,n)
        else if(itype == 1 .and. ddtype == 'u') then
          n=14
          nk=43
          call read_data(dtype,n,nk)
        else if (itype == 0 .and. ddtype /= 'u') then
           n=5
           call read_data_sfc(dtype,n) 
        else if (itype == 1 .and. ddtype == 'q') then
           n=5
           nk=29
          call read_data(dtype,n,nk)
        else if (itype == 1 .and. ddtype == 't') then
           n=5
           nk=43
          call read_data(dtype,n,nk)
        endif



        stop
        end
