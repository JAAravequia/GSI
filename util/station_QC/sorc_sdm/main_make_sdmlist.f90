!  the program read all three month data and put into one file for
!  statistics

    character(5) dtype,atype 
    integer type
    character(3) ddtype
    character(20) filein,fileout
    character(20),dimension(4):: file
    character(5),dimension(4):: dtypearr 
    logical,dimension(4) :: filexist
    integer,dimension(4) :: itype
    character(8),dimension(7000) :: stdid
    integer,dimension(7000) :: iqcflg 
    integer,dimension(200,33) :: iqcflg_sond 
    real(4),dimension(200,33) :: pressure 
    character(8),dimension(4,7000) :: stdidall
    integer,dimension(4,7000) :: iqcflgall 
    integer,dimension(4) :: ncount_all


    namelist/input/type,itype,filexist


     read(5,input)
     write(6,input)
     write(6,*)' '

    write(ddtype,'(i3)') type 
      

      file(1)='ps'//trim(ddtype)//'_rej_list'
      file(2)='t'//trim(ddtype)//'_rej_list'
      file(3)='q'//trim(ddtype)//'_rej_list'
      file(4)='uv'//trim(ddtype)//'_rej_list'
      dtypearr(1)='ps'//trim(ddtype)
      dtypearr(2)='t'//trim(ddtype)
      dtypearr(3)='q'//trim(ddtype)
      dtypearr(4)='uv'//trim(ddtype)
      ncount=0
      ncount_all=0

     if (type ==120) then
         fileout=trim(ddtype)//'_emcrej_list'
        do i=1,4
           if(filexist(i)) then
              filein=file(i)
              dtype=dtypearr(i)
              if(i ==1 .and. itype (i) ==0) then
                 atype='P----'
                 call read_list_sfc(dtype,filein,2,7000,ncount,stdid,iqcflg)
                 open(21,file=fileout,form='formatted')
                 write(21,100)
                 do j=1,ncount
                    write(21,110)stdid(j),atype
                 enddo
              else if(i /=4 .and. itype(i) ==1) then
                 if (i ==2 .and. itype(i) ==1) atype='--P--'
                 if (i ==3 .and. itype(i) ==1) atype='---P-'
                 call make_sdm_list(fileout,dtype,filein,2,atype)
              else if( i == 4 .and. itype(i) ==1) then
                 atype='----P'
                 call make_sdm_list(fileout,dtype,filein,4,atype)
              endif
           endif
        enddo
     endif 
   if( type /=120) then
      nfile=0
      do i=1,4
         if(filexist(i) .and. itype(i) ==0) then
            nfile=nfile+1
            filein=file(i)
            dtype=dtypearr(i) 
            if ( i /=4) then
               call read_list_sfc(dtype,filein,2,7000,ncount,stdid,iqcflg) 
               stdidall(i,1:ncount)=stdid(1:ncount)
               iqcflgall(i,1:ncount)=iqcflg(1:ncount)
               ncount_all(i)=ncount
            else if ( i ==4) then
               call read_list_sfc(dtype,filein,4,7000,ncount,stdid,iqcflg) 
               stdidall(i,1:ncount)=stdid(1:ncount)
               iqcflgall(i,1:ncount)=iqcflg(1:ncount)
               ncount_all(i)=ncount
            endif
         else if(filexist(i) .and. itype(i) ==1) then
            filein=file(i)
              dtype=dtypearr(i)
              if(i == 1) then
                 atype='P----'
                 fileout='ps'//trim(ddtype)//'_emcrej_list'
              else if(i ==2) then
                 atype='--P--'
                 fileout='t'//trim(ddtype)//'_emcrej_list'
              else if(i ==3) then
                 atype='---P-'
                 fileout='q'//trim(ddtype)//'_emcrej_list'
              else if(i ==4) then
                 atype='----P'
                 fileout='uv'//trim(ddtype)//'_emcrej_list'
              endif
              if( i /=4) then
                 call make_sdm_list(fileout,dtype,filein,2,atype)
              else if( i ==4) then
                 call make_sdm_list(fileout,dtype,filein,4,atype)
              endif
         endif
      enddo

       fileout=trim(ddtype)//'_emcrej_list'
 
       print *,'nfile=',nfile

       if(nfile == 0) then
         stop 
       else if(nfile ==1 ) then
!      fileout=trim(ddtype)//'_rej_list'
       open(21,file=fileout,form='formatted')
       write(21,100)
          do i=1,4
             if (filexist(i) .and. itype(i) ==0) then 
                if(i == 1) then
                   atype='P----'
                else if(i ==2) then
                   atype='--P--'
                else if(i ==3) then
                   atype='---P-'
                else if(i ==4) then
                   atype='----P'
                endif
                nobs=ncount_all(i)
                do j=1,nobs
                    if(type ==181 .or. type ==187 .or. type ==281 .or. type ==287) then
                       write(21,120) stdidall(i,j),atype
                    else if ( type == 180 .or. type == 280) then
                       write(21,130) stdidall(i,j),atype  
                    else if ( type == 188 .or. type == 288) then
                       write(21,140) stdidall(i,j),atype  
                    endif
                enddo
             endif
          enddo  
       else if (nfile  ==2) then 
          call compare_two_sfc(type,fileout,stdidall,ncount_all,filexist,itype,7000) 
       else if (nfile ==3) then
          call compare_three_sfc(type,fileout,stdidall,ncount_all,filexist,itype,7000) 
       else if (nfile ==4) then
          call compare_four_sfc(type,fileout,stdidall,ncount_all,filexist,itype,7000)
       endif 

  endif                
                
              
100 format('STATION',3x,'N_LAT',2x,'W_LON',2x,'YYYYMMDDHH',2x,&
           'TYP  PZTQW  LTYP  PRESSURE(S)  MSG_TYPE  WMO_BULLHDR  LAT_LON_BDRY  ITP  NCEP-ANL-NETWORK(S)')
110 format(a8,2x,'-----  -----  ----------',2x,'UPA',2x,a5,2x,&
           'SURF  -----------  --------  -----------  ------------',&
            2x,'---  -------------------')
120 format(a8,2x,'-----  -----  ----------',2x,'SFC',2x,a5,2x,&
           '----  -----------  --------  -----------  ------------',&
           2x,'---  -------------------')
130 format(a8,2x,'-----  -----  ----------',2x,'SHP',2x,a5,2x,&
           '----  -----------  --------  -----------  ------------',&
           2x,'---  -------------------')
140 format(a8,2x,'-----  -----  ----------',2x,'MSO',2x,a5,2x,&
           '----  -----------  --------  -----------  ------------',&
           2x,'---  -------------------')

        stop
        end
