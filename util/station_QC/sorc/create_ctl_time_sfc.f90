
     subroutine creat_ctl_time_sfc(dtype,nob,nobs,n,ntime,stdid,cid,std,rmean,xmd,&
                               yr,mon,day,hr,shr,rmiss) 

! $$$  subprogram documentation block
! subprogram:  creat_time_sfc
!  prgmmr:  x.su                      date: 2012-05-01
!                .      .    .

!!  the program creat control file with time series for the surface station
    
   implicit none

     character(len=8),dimension(nob) ::  stdid
     character(4) yr,shr
     character(3) mon
     character(2) day,hr
     character(5) dtype
     real(4), dimension(nob,6) :: cid
     real(4),dimension(nobs,n) :: rmean,std,xmd
     integer nob,nobs,n,i,ntime
     real(4) rmiss

     character(5) ddtype
     character(20) ctlfile,tstring
     character(len=8)  stdid_tmp



    ctlfile=trim(dtype)//'_time.ctl'
    tstring=trim(hr)//'Z'//trim(day)//trim(mon)//trim(yr)//' '//trim(shr)

   print *,'creat_time_sfc:',tstring

  

  
   ddtype=dtype(1:1)

   print *,dtype,ctlfile
   print *,nob,nobs

   print *, cid(1,1),cid(1,2),cid(1,3),cid(1,4),cid(1,5),cid(1,6)
   print *, rmean(11,1),rmean(2,5),rmean(3,5),rmean(4,5),rmean(5,5)
   print *, std(11,1),std(11,2),std(11,3),std(11,4),std(11,5)

    open(11,file=ctlfile,form='formatted')

    write(11,100)
    write(11,110)
    write(11,120) rmiss
    write(11,130) dtype  
    write(11,140) nobs 
    do i=1,nobs
     stdid_tmp=stdid(i)
     stdid_tmp=stdid_tmp(1:7)
     if( i ==1) print *, 'stdid_tmp=',stdid_tmp
    if(trim(ddtype) == 'u') then
     write(11,155) i,stdid_tmp,cid(i,1),cid(i,2),cid(i,3),rmean(i,1),std(i,1),rmean(i,5),std(i,5),xmd(i,3),xmd(i,1)
     write(11,156) i,stdid_tmp,cid(i,1),cid(i,2),cid(i,3),rmean(i,3),std(i,3),rmean(i,6),std(i,6),xmd(i,3),xmd(i,1)
    else
     write(11,150) i,stdid_tmp,cid(i,1),cid(i,2),cid(i,3),rmean(i,1),std(i,1),rmean(i,5),std(i,5),xmd(i,3),xmd(i,1)
    endif
    enddo 
    write(11,160)  
    write(11,171)
    write(11,180) ntime,tstring
    if(trim(ddtype) == 'u' ) then
     write(11,300) 
     write(11,310) 
     write(11,320) 
     write(11,330) 
     write(11,340) 
     write(11,350) 
     write(11,360) 
     write(11,370) 
     write(11,380) 
     write(11,390) 
     write(11,400) 
     write(11,270) 
  else
    write(11,190)
    write(11,200) 
    write(11,210)
    write(11,220) 
    write(11,230)
    write(11,240)
    write(11,250)    
    write(11,260)    
    write(11,270)     
 endif
   
  

100 format('dset ^')
110 format('options big_endian sequential')
120 format('undef ',f5.0)
130 format('title ',a5, 1x,'time series') 
140 format('xdef ',i8,' linear 1.0 1.0')
150 format('* x= ',i8,' sid= ',a8,' lon= ',f6.2,' lat= ',f6.2,' sht= ',f5.0,&
          ' O-B= ',f4.1,' RMS= ',f4.1,' Obs= ',f6.1,' std= ',f5.1,' Mad= ',f4.1,' Med= ',f4.1)
155 format('* xd= ',i8,' dsid= ',a8,' lon= ',f6.2,' lat= ',f6.2,' sht= ',f5.0,&
          ' O-Bd= ',f6.1,' RMSd= ',f6.1,' obs= ',f6.1,' std= ',f6.1,' Mad= ',f6.1,' Med= ',f6.1)
156 format('* xs= ',i8,' ssid= ',a8,' lon= ',f6.2,' lat= ',f6.2,' sht= ',f5.0,&
        ' O-Bs= ',f6.1,' RMS= ',f6.1,' OBS= ',f6.1,' std= ',f6.1,' Mad= ',f6.1,' Med= ',f6.1)
160 format('ydef  1  linear 1.0 1.0')
171 format('zdef 1 linear 1.0 1.0') 
180 format('tdef  ',i5,' linear ',a20)
300 format('vars  10')
310 format('obg     0     99     obs-bac(direction)')
320 format('oba     0     99     obs-anl(direction)')
330 format('obgv     0     99     obs-bac(speed)')
340 format('obav     0     99     obs-anl(speed)')
350 format('obd      0     99     obs(direction)')
360 format('obs      0     99     obs(speed)')
370 format('vqcg     0     99     variational qc(bac)')
380 format('vqca     0     99     variational qc(anl)')
390 format('no       0     99      assimilated no')
400 format('rjno     0     99      rejected by gross check')
190 format('vars  7')
200 format('obg     0     99     obs-bac')
210 format('oba     0     99     obs-anl')
220 format('vqcb    0      99     variational qc(bac)')
230 format('vqca    0     99     variational qc(anl)')
240 format('obs     0     99     observations')
250 format('no      0      99    *  assimilated')
260 format('rjno    0      99    *  rejected by gross check')
270 format('endvars')


close(11)

return
end
