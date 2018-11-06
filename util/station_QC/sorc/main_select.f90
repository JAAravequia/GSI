!  the program read all three month data and put into one file for
!  statistics


    character(5) dtype 
    integer itype
    integer ntype
    character(1) ddtype
    integer,dimension(4) :: iqcflg

!  iqcflg: 1, rejected because of 80% rejected by GSI gross check,2: rejected
!  because observation constant value,3: rejected by rejection criteria,4:
!  normal or bias correction


    namelist/input/dtype,itype


     read(5,input)
     write(6,input)
     write(6,*)' '

       print *,'call select begin'

        ddtype=dtype(1:1)
        ddtype=trim(ddtype)
        iqcflg(1)=1
        iqcflg(2)=2
        iqcflg(3)=3
        iqcflg(4)=0

        print *,'ddtype ',ddtype,itype

        if(itype == 0 .and. ddtype == 'u') then
          print *,'call w_selet.sfc ', dtype
          call w_selet_sfc(dtype,iqcflg)
        else if(itype == 1 .and. ddtype == 'u') then
          print *,'call w_selet ', dtype
          nk=43
          call w_selet(dtype,nk,iqcflg)
        else if (itype == 0 .and. ddtype == 'q') then
          print *,'call stas_sfc ', dtype
          call q_selet_sfc(dtype,iqcflg)
          print *,'call stas_sfc ', dtype
        else if (itype == 1 .and. ddtype == 'q') then
          print *,'q_selet ', dtype
           nk=29
           call q_selet(dtype,nk,iqcflg)
        else if (itype == 1 .and. ddtype == 't') then
          print *,'t_selet ', dtype
           nk=43
           call t_selet(dtype,nk,iqcflg)
        else if (itype == 0 .and. ddtype == 't') then
          print *,'t_selet_sfc ', dtype
           call t_selet_sfc(dtype,iqcflg)
        else if (itype == 0 .and. ddtype == 'p') then
          print *,'ps_selet ', dtype
           call ps_selet(dtype,iqcflg)
        endif



        stop
        end
