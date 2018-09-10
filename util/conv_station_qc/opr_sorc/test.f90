!  this is test

   character(len=50) :: hdstr,obstr
   real(8),dimension(4,5) :: obs
   real(8),dimension(8) :: hdr
   character(8) subset,stationid,sub,cdtype

   data obstr /'POB PQM PRC PPC'/
   data hdstr  /'SID XOB YOB ELV T29 ITP TYP DHR'/
   data lunin/11/

 nread=0
 ndata=0
 nrec=0
   open(lunin,file='prepbufr.post',form='unformatted')
   call openbf(lunin,'IN',lunin)
   do while(ireadmg(lunin,subset,idate).eq.0)
    if(subset .ne. 'ADPSFC') cycle
    do while(ireadsb(lunin).eq.0)
     nread=nread+1
      call ufbint(lunin,hdr,8,1,iret,hdstr)   ! read header from one observation
      call ufbevn(lunin,obs,4,1,5,iret,obstr)  !  read different  stacks 
      if(hdr(7) == 181.0) then
       ndata=ndata+1
!        if(obs(2,1) /= obs(2,2)) then
          write(6,100) hdr(1),hdr(2),hdr(3),hdr(4),hdr(5),hdr(6),hdr(7),hdr(8) 
          write(6,100) obs(1,1),obs(1,2),obs(2,1),obs(2,2),obs(3,1),obs(3,2),obs(4,1),obs(4,2)
          nrec=nrec+1
!        endif
      endif
    enddo
 enddo

100  format(8f10.2)

          print *,nread,ndata,nrec

    stop
    end
