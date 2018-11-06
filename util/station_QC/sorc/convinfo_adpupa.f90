!   the subroutine to read convention information file

    subroutine convinfo_adpupa(gtross2)

   character(120):: crecord
  character(1) :: cflg
  character(7):: iotype

  integer(4):: ittype,ituse,ntumgrp,ntgroup,ntmiter,isubtype
  integer(4) :: lunin,ithin,npred
  real(8) :: ttwind,gtross,etrmax,etrmin,vtar_b,vtar_pg,rmesh,pmesh
  real(4),dimension(4) :: gtross2


  lunin=11

!  print *, 'start coninfo subroutine'
  open(lunin,file='convinfo',form='formatted')
  rewind(lunin)

  loopd: do
       read(lunin,1030,IOSTAT=iflag)cflg,iotype,crecord
       if(cflg == '!')cycle
       if( iflag /= 0 ) exit loopd
       read(crecord,*)ittype,isubtype,ituse,ttwind,ntumgrp,ntgroup,ntmiter,&
                      gtross,etrmax,etrmin,vtar_b,vtar_pg,ithin,rmesh,pmesh,npred

      if( ittype == 120 ) then
        if(trim(iotype) == 'ps') then
           gtross2(1)=gtross
        else if(trim(iotype) == 'q') then
           gtross2(2)=gtross
        else if(trim(iotype) == 't') then
           gtross2(3)=gtross
        endif
      else if( trim(iotype) == 'uv' .and.  ittype == 220) then
           gtross2(4)=gtross 
      endif
  enddo  loopd

1030 format(a1,a7,2x,a120)

     return
     end
