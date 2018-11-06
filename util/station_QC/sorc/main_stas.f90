!  the program read all three month data and put into one file for
!  statistics


    character(5) dtype 
    character(3)  mon 
    character(4)  shr,yr 
    character(2)  hr,day
    integer itype,ntype,iyr,iday,ihr
    character(1) ddtype
    integer,dimension(4) :: iqcflg

!  iqcflg: 1, rejected because of 80% rejected by GSI gross check,2: rejected
!  because observation ocnstant value,3: rejected by rejection criteria,4:
!  normal or bias
    
  


    namelist/input/dtype,itype,yr,mon,day,hr,shr


     read(5,input)
     write(6,input)
     write(6,*)' '


     iqcflg(1)=1
     iqcflg(2)=2
     iqcflg(3)=3
     iqcflg(4)=0

       print *,'call stas begin'

        ddtype=dtype(1:1)
        ddtype=trim(ddtype)

        print *,'ddtype ',ddtype,itype
        print *,yr,mon,day,hr,shr

        if(itype == 0 .and. ddtype == 'u') then
          print *,'call stas_sfc_spdir ', dtype
          call stas_sfc_spdir(dtype,yr,mon,day,hr,shr,iqcflg)
        else if(itype == 1 .and. ddtype == 'u') then
          print *,'call stas_spdir ', dtype
          nk=43
          call stas_spdir(dtype,nk,yr,mon,day,hr,shr)
        else if (itype == 0 .and. ddtype /= 'u') then
          print *,'call stas_sfc ', dtype
          call stas_sfc(dtype,yr,mon,day,hr,shr,iqcflg)
          print *,'call stas_sfc ', dtype
        else if (itype == 1 .and. ddtype == 'q') then
          print *,'call stas ', dtype
           nk=29
           call stas(dtype,nk,yr,mon,day,hr,shr)
        else if (itype == 1 .and. ddtype == 't') then
          print *,'call stas ', dtype
           nk=43
           call stas(dtype,nk,yr,mon,day,hr,shr)
        endif



        stop
        end
