!    read the bias and rejection list

     
     subroutine make_sdm_list(fileout,dtype,file,nd,atype)
    
    
     character(5) dtype,atype
     character(8) stationid
     character(20) file,fileout
     real(4) rlon,rlat,stdno,rmeanb,rmeana,stdb,stda,tdatano
     real(4) tdatano2,xmedb,xmeda,smadb,smada,breakb1,breakb2,breake1,breake2
     logical :: lexist 
     


    inquire(file=fileout,exist=lexist)
    if(.not. lexist) then 
      open(21,file=fileout,status='new',action='write',form='formatted')
      write(21,200) 
    endif

 ncount =0

     open (11,file=file,form='formatted',iostat=ierror)
!  if(ierror ==0) then
     loopd: do
      read(11,100,IOSTAT=iflag) stationid,rlon,rlat,nlev 
       ncount=ncount+1
      if(iflag /=0 ) exit
      do k=1,nlev
        if(nd ==2) then
           read(11,110) plve1,plve2,rmeanb,rmeana,stdb,stda,iqc
        else if(nd ==4) then
           read(11,120) plve1,plve2,rmeanbw,rmeanaw,rmeanbs,rmeanas,stdbw,&
                        stdaw,stdbs,stdas,iqcflg
        endif
        iplve1=int(plve1) 
        iplve2=int(plve2) 
        write(21,210) stationid,atype,iplve1,iplve2
      enddo
     enddo   loopd

! endif      

100 format(2x,a8,2f8.2,i8)
110 format(2x,6f8.2,i3)
120 format(2x,10f8.2,i3)

200 format('STATION',3x,'N_LAT',2x,'W_LON',2x,'YYYYMMDDHH',2x,&
           'TYP  PZTQW  LTYP  PRESSURE(S)  MSG_TYPE  WMO_BULLHDR',2x,&
           'LAT_LON_BDRY  ITP  NCEP-ANL-NETWORK(S)')
210 format(a8,2x,'-----  -----  ----------',2x,'UPA',2x,a5,2x,&
           '----  ',i4,1x,i4,'--  --------  -----------  ------------',&
           2x,'---  -------------------') 


    print *,' make_sdm_list:file,ncount= ',file,ncount


       return
       end
 
