!  this is the main program to read prepbufr file to look at 
! seperate individual station type 

  
     character*8 sub
     character*5 dtype
     character*1 ddtype
     integer itype,jsubtype,iuse
     real(8) gross
     real(4)  rtype




      namelist/input/sub,dtype,itype,rtype

      read(5,input)
     write(6,input)
     write(6,*)' '

       ddtype=dtype(1:1)
        call convinfo_read(dtype,gross,iuse)
       print *,dtype,gross,iuse,ddtype
     

     if(sub == 'ADPUPA' .and. dtype /= 'uv221' ) then
        call  read_prepbufr_raw(sub) 
     else if (sub == 'ADPUPA' .and. dtype == 'uv221' ) then 
        call  read_prepbufr_wind(sub,dtype,rtype,gross) 
     else if(sub == 'PROFLR' .or. sub == 'VADWND' .and. iuse >0) then 
        call  read_prepbufr_wind(sub,dtype,rtype,gross) 
     else if(sub == 'PROFLR' .or. sub == 'VADWND' .and. iuse <0) then 
        call  read_prepbufr_wind_mor(sub,dtype,rtype,gross) 
     else if( sub == 'ADPSFC' .and. ddtype == 'p' .and. iuse >0) then 
        print *,'call read_prepbufr_sfc_ps'
        call  read_prepbufr_sfc_ps(sub,dtype,rtype,gross) 
        print *,' end of call read_prepbufr_sfc_ps'
     else if( sub == 'ADPSFC' .and. ddtype == 'p' .and. iuse <0) then 
        call  read_prepbufr_sfc_ps_mor(sub,dtype,rtype,gross) 
     else if( sub == 'ADPSFC' .and. ddtype == 'q' .and. iuse >0) then 
        call  read_prepbufr_sfc_q(sub,dtype,rtype,gross) 
     else if( sub == 'ADPSFC' .and. ddtype == 'q' .and. iuse <0) then 
        call  read_prepbufr_sfc_q_mor(sub,dtype,rtype,gross) 
     else if( sub == 'ADPSFC' .and. ddtype == 't' .and. iuse >0) then 
        call  read_prepbufr_sfc_t(sub,dtype,rtype,gross) 
     else if( sub == 'ADPSFC' .and. ddtype == 't' .and. iuse <0) then 
        call  read_prepbufr_sfc_t_mor(sub,dtype,rtype,gross) 
     else if(sub == 'ADPSFC' .and. ddtype == 'u' .and. iuse >0) then
        call  read_prepbufr_sfc_w(sub,dtype,rtype,gross) 
     else if(sub == 'ADPSFC' .and. ddtype == 'u' .and. iuse <0) then
        call  read_prepbufr_sfc_w_mor(sub,dtype,rtype,gross) 
     else if( sub == 'SFCSHP' .and. ddtype == 'p') then
       call  read_prepbufr_sfc_ps(sub,dtype,rtype,gross) 
     else if(sub == 'SFCSHP' .and. ddtype == 't') then
       call  read_prepbufr_sfc_t(sub,dtype,rtype,gross) 
     else if(sub == 'SFCSHP' .and. ddtype == 'q') then
       call  read_prepbufr_sfc_q(sub,dtype,rtype,gross) 
     else if(sub == 'SFCSHP' .and. ddtype == 'u') then
       call  read_prepbufr_sfc_w(sub,dtype,rtype,gross) 
     else if (sub == 'MSONET' .and. ddtype == 't') then
       call  read_prepbufr_sfc_t(sub,dtype,rtype,gross)
     else if (sub == 'MSONET' .and. ddtype == 'p') then
       call  read_prepbufr_sfc_ps(sub,dtype,rtype,gross)
     else if(sub == 'MSONET' .and. ddtype == 'q') then
       call  read_prepbufr_sfc_q(sub,dtype,rtype,gross)
     else if(sub == 'MSONET' .and. ddtype == 'u') then
       call  read_prepbufr_sfc_w(sub,dtype,rtype,gross)
     endif
    

     stop
     end
