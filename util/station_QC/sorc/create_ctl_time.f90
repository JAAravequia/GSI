
     subroutine creat_ctl_time(dtype,nlev,nob,nobs,ntime,stdid,cid,plevel,&
                           yr,mon,day,hr,shr,rmiss) 
! $$$  subprogram documentation block
! subprogram:  creat_time 
!  prgmmr:  x.su                      date: 2012-05-01
!                .      .    .

!!  the program creat control file with time series

  implicit none
    

! Declare passed variables
     character(len=8),dimension(nob) ::  stdid
     character(len=8)  stdid_tmp
     real(4), dimension(nob,6) :: cid
     character(5) dtype,ddtype
     character(4) yr,shr 
     character(3) mon
     character(2) day,hr
     character(20) ctlfile,tstring

     real(4), dimension(nlev) :: plevel
     integer,dimension(nlev) :: iplev
     integer nobs,nob,ntime,nlev
     real(4) rmiss 

! Declare local variables
     integer i
    


    ctlfile=trim(dtype)//'_time.ctl'

     do i=1,nlev
     iplev(i) = nint(plevel(i))
    enddo

   tstring=trim(hr)//'Z'//trim(day)//trim(mon)//trim(yr)//' '//trim(shr)

   print *,tstring

   print *,dtype,ctlfile
   print *, cid(1,1),cid(1,2),cid(1,3),cid(1,4),cid(1,5),cid(1,6)

   ddtype=dtype(1:1)

    open(11,file=ctlfile,form='formatted')

    write(11,100)
    write(11,110)
    write(11,120) rmiss
    write(11,130) dtype  
    write(11,140) nobs 
    do i=1,nobs
     stdid_tmp=stdid(i)
     stdid_tmp=stdid_tmp(1:7)
     write(11,150) i,stdid_tmp,cid(i,1),cid(i,2),cid(i,3),cid(i,4),cid(i,5) 
    enddo 
    write(11,160)  
    write(11,170) nlev
    write(11,175) (iplev(i),i=1,nlev)
    write(11,180) ntime,tstring
    if(trim(ddtype) == 'u' ) then
     write(11,300)  
     write(11,310) nlev 
     write(11,320) nlev 
     write(11,330) nlev 
     write(11,340) nlev 
     write(11,350) nlev 
     write(11,360) nlev 
     write(11,370) nlev 
     write(11,380) nlev 
     write(11,390) nlev 
     write(11,400) nlev 
     write(11,270) 
  else
    write(11,190) 
    write(11,200) nlev 
    write(11,210) nlev
    write(11,220) nlev 
    write(11,230) nlev
    write(11,240) nlev
    write(11,250) nlev
    write(11,260) nlev    
    write(11,270)     
 endif
   
  

100 format('dset ^')
110 format('options big_endian sequential')
120 format('undef ',f5.0)
130 format('title ',a5, 1x,'time series') 
140 format('xdef ',i8,' linear 1.0 1.0')
150 format('* x= ',i8,' stationid= ',a8,' lon= ',f6.2,' lat= ',f6.2,&
            ' sheight= ',f6.1, ' stype= ',f4.1,' instype= ',f4.1)
160 format('ydef  1  linear 1.0 1.0')
170 format('zdef ',i5,'  levels')
175 format(43i5)
180 format('tdef  ',i5,' linear ',a20)
300 format('vars  10')
310 format('obg     ',i5,'   99    obs-bac(direction)')
320 format('oba    ',i5,'    99       obs-anl(direction)')
330 format('obgv    ',i5,'    99     obs-bac(speed)')
340 format('obav    ',i5,'    99     obs-anl(speed)')
350 format('obd     ',i5,'    99     obs(direction)')
360 format('obs     ',i5,'    99     obs(speed)')
370 format('vqcg    ',i5,'    99     variational qc(bac)')
380 format('vqca    ',i5,'    99     variational qc(anl)')
390 format('no      ',i5,'    99      assimilated no')
400 format('rjno    ',i5,'    99      rejected by gross check')
190 format('vars  7')
200 format('obg    ',i5,'    99     obs-bac')
210 format('oba    ',i5,'    99     obs-anl')
220 format('vqcb   ',i5,'    99     variational qc(bac)')
230 format('vqca   ',i5,'    99     variational qc(anl)')
240 format('obs    ',i5,'    99     observations')
250 format('no     ',i5,'    99    *  assimilated')
260 format('rjno   ',i5,'    99    *  rejected by gross check')
270 format('endvars')


close(11)

return
end
