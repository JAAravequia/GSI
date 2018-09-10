!!  the program creat control file with time series

    
     subroutine creat_stas_hist_ctl(dtype,plev,nlev,nhis,factor,factor2,rmiss) 

      real(4), dimension(nlev) :: plev
      integer,dimension(nlev) :: iplev
     character(5) dtype,ddtype
     character(20) ctlfile



    ctlfile=trim(dtype)//'_stas_hist.ctl'

   print *,dtype,ctlfile,nlev

   ddtype=dtype(1:1)

    do i=1,nlev
     iplev(i) = nint(plev(i))
    enddo

    open(11,file=ctlfile,form='formatted')

    write(11,100)
    write(11,110)
    write(11,120) rmiss
    write(11,130) dtype  
    write(11,140) nhis 
  if(trim(ddtype) == 'u') then
    write(11,141) factor,factor2
  else
    write(11,145)  factor 
  end if
      write(11,150) 
    if( nlev ==1 )  then
    write(11,161) 
    mlev=0
   else
     mlev=nlev
      write(11,160) nlev 
     write(11,170) (iplev(i),i=1,nlev)
   endif
      write(11,180) 
  if(trim(ddtype) == 'u') then
    write(11,190)  
    write(11,200) mlev 
    write(11,210) mlev 
    write(11,220) mlev 
    write(11,230) mlev 
    write(11,240) mlev 
    write(11,250) mlev 
    write(11,260) mlev 
    write(11,270) mlev 
  else
    write(11,300) 
    write(11,310) mlev
    write(11,320) mlev
    write(11,330) mlev
    write(11,340) mlev
  endif
   write(11,400)
100 format('dset ^')
110 format('options big_endian sequential')
120 format('undef ',f6.1)
130 format('title ',a5, 1x,'histogram') 
140 format('xdef ',i4,' linear 1.0 1.0')
141 format('*x interval direction ',f4.1,2x,f4.1)
145 format('*x interval ',f4.1)
150 format('ydef  1  linear 1.0 1.0')
160 format('zdef ',i5,'  levels') 
161 format('zdef 1  linear 1.0 1.0') 
170 format(43i5)
180 format('tdef  1 linear 00z22jun2006 12hr   ')
190 format('vars  8')
200 format('rmd    ', i5, '    99    * mean direction')
210 format('rmd_ra ', i5, '    99    * mean direction frequency ratio')
220 format('rstd    ',i5,'     99     * standard deviation direction')
230 format('rstd_ra ',i5,'     99     * standard deviation direction ratio')
240 format('rms     ',i5,'     99     * mean speed')
250 format('rms_ra  ',i5,'     99     * mean speed ratio')
260 format('rsts    ',i5,'     99     * standard deviation speed')
270 format('rsts_ra ',i5,'     99     * standard deviation speed ratio')
300 format('vars  4')
310 format('rm    ', i5, '    99    * mean ')
320 format('rm_ra ', i5, '    99    * mean  frequency ratio')
330 format('rst    ',i5,'     99     * standard deviation ')
340 format('rst_ra ',i5,'     99     * standard deviation  ratio')
400 format('endvars')

close(11)

return
end
