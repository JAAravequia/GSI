!!  the program creat control file with time series

    
     subroutine creat_ctl_station(dtype,nlev,plevel,rmiss) 


      real(4), dimension(nlev) :: plevel
      integer,dimension(nlev) :: iplev
     character(5) dtype,ddtype
     character(25) ctlfile



    ctlfile=trim(dtype)//'_stas_station.ctl'

   print *,dtype,ctlfile

    ddtype=dtype(1:1)

    do i=1,nlev
     iplev(i) = nint(plevel(i))
    enddo

    open(11,file=ctlfile,form='formatted')

    write(11,100)
    write(11,105)
    write(11,110)
    if(trim(ddtype) == 'u' .or. trim(ddtype) == 'p') then
       write(11,116) dtype
    else
       write(11,117) dtype
    endif 
    write(11,120) rmiss
    write(11,130) dtype  
     write(11,180)
     if(nlev ==1)  then
       mlev=0
     else
      mlev=1
     endif
      print *,ctlfile,mlev,nlev
     write(11,170) (iplev(i),i=1,nlev)
    if( trim(ddtype) == 'u') then
     write(11,190)
     write(11,200) mlev
     write(11,210) mlev
     write(11,215) mlev
     write(11,220) mlev
     write(11,221) mlev
     write(11,222) mlev
     write(11,225) mlev
     write(11,230) mlev
     write(11,235) mlev
     write(11,240) mlev
     write(11,241) mlev
     write(11,242) mlev
     write(11,245) mlev
     write(11,250) mlev
     write(11,260) mlev
     write(11,270) mlev
     write(11,280) mlev
     write(11,290) mlev
  else
     write(11,390)
     write(11,400) mlev
     write(11,410) mlev
     write(11,411) mlev
     write(11,420) mlev
     write(11,430) mlev
     write(11,431) mlev
     write(11,440) mlev
     write(11,450) mlev
     write(11,460) mlev
     write(11,470) mlev
     write(11,480) mlev
     write(11,490) mlev
  endif
    
    
     write(11,300) 
  

100 format('dset ^')
105 format('DTYPE  station')
110 format('options big_endian sequential')
116 format('STNMAP ',a5,'.map')
117 format('STNMAP ',a4,'.map')
120 format('undef ',f5.0)
130 format('title ',a5, 1x,'statistics') 
170 format('*zdef ',43i4) 
180 format('tdef  1 linear 00z22jun2006 12hr   ')
190 format('vars  18')
200 format('rmdirb ', i5,'     99     * mean(O-B)(direction)')
210 format('rmdira ', i5,'     99     * mean(O-A)(direction)')
215 format('rmspdb ', i5,'     99     * mean(O-B)(speed)')
220 format('rmspda ', i5,'     99     * mean(O-A)(speed)')
221 format('rmdir  ', i5,'     99     * mean(direction)')
222 format('rmsp   ', i5,'     99     * mean(speed)')
225 format('rsddirb ', i5,'   99     * standard deviation(O-B)direction')
230 format('rsddira ', i5,'   99     * standard deviation(O-A)direction')
235 format('rsdspdb ', i5,'   99     * standard deviation(O-B)speed')
240 format('rsdspda ', i5,'   99     * standard deviation(O-A)speed')
241 format('rsddir ', i5,'   99     * standard deviation direction')
242 format('rsdsp ', i5,'   99     * standard deviationspeed')
245 format('xmedb  ', i5,'     99     * median(O-B)direction')
250 format('xmeda  ', i5,'     99     * median(O-B)speed')
260 format('smadb  ', i5,'     99     * median departure(O-B)direction')
270 format('smada  ', i5,'     99     * median departure(O-B)speed')
280 format('nob    ', i5,'     99     *  number(B)')
290 format('no_rej ', i5,'     99     *  data rejected by quality control')
390 format('vars  12')
400 format('rmeanb ', i5,'     99     * mean(O-B)')
410 format('rmeana ', i5,'     99     * mean(O-A)')
411 format('rmean ', i5,'     99     * mean obs')
420 format('rstdb  ', i5,'     99     * standard deviation(O-B)')
430 format('rstda  ', i5,'     99     * standard deviation(O-A)')
431 format('rstd  ', i5,'     99      * standard deviation obs')
440 format('xmedb  ', i5,'     99     * median(O-B)')
450 format('xmeda  ', i5,'     99     * median(O-A)')
460 format('smadb  ', i5,'     99     * median departure(O-B)')
470 format('smada  ', i5,'     99     * median departure(O-A)')
480 format('nob    ', i5,'     99     *  number(B)')
490 format('no_rej ', i5,'     99     *  data rejected by quality control')
300 format('ENDVARS')

close(11)

return
end
