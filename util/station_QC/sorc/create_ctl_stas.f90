!!  the program creat control file with time series

    
     subroutine creat_ctl_stas(dtype,nlev,nob,nobs,stdid,cid,plevel,rmiss) 

     character(len=8),dimension(nob) ::  stdid
     character(len=8)  stdid_tmp
     
     real(4), dimension(nob,6) :: cid

      real(4), dimension(nlev) :: plevel
      integer,dimension(nlev) :: iplev
     character(5) dtype,ddtype
     character(20) ctlfile



    ctlfile=trim(dtype)//'_stas.ctl'

   print *,dtype,ctlfile,nlev
   print *, cid(1,1),cid(1,2),cid(1,3),cid(1,4),cid(1,5),cid(1,6)

   ddtype=dtype(1:1)

    do i=1,nlev
     iplev(i) = nint(plevel(i))
    enddo

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
     if( i==4) print *,cid(i,1),cid(i,2),cid(i,3),cid(i,4),cid(i,5)
    enddo 
  if(trim(ddtype) == 'u') then
    write(11,160) 
  else
    write(11,161) 
  endif
    write(11,165) 
    write(11,166) 
    write(11,167) 
    write(11,168) 
    write(11,169) 
    if( nlev ==1 )  then
    mlev=0
    write(11,171) 
   else
     mlev=nlev
    write(11,170) nlev
     write(11,175) (iplev(i),i=1,nlev)
   endif
    write(11,180)
      write(11,190) 
      write(11,200) mlev 
      write(11,210) mlev 
      write(11,220) mlev 
      write(11,230) mlev 
      write(11,240) mlev  
      write(11,250)   
  

100 format('dset ^')
110 format('options big_endian sequential')
120 format('undef ',f5.0)
130 format('title ',a5, 1x,'statistics') 
140 format('xdef ',i4,' linear 1.0 1.0')
150 format('* x= ',i5,' stationid= ',a8,' lon= ',f6.2,' lat= ',f6.2,&
            ' sheight= ',f6.1, ' stype= ',f4.1,' instype= ',f6.1)
160 format('ydef  6  linear 1.0 1.0')
161 format('ydef  5  linear 1.0 1.0')
165 format('**  ydef, 1: obs-bac, 2. obs-anl, 3, vqc(bac),4. vqc(anl)')
166 format('**  ydef, for median statis: 1,2 for median, 3,4 for median departure )')
167 format('**  ydef, 5 for obsevation,for wind 5: for dir., 6: fore speed )')
168 format('** if wind, ydef, 1: obs-bac(dir), 2. obs-anl(dir), 3, o-b(speed),4.o-a (speed)')
169 format('** if wind, ydef, for median statis: 1,2 for median, 3,4 for median departure(for all o-b ) )')
170 format('zdef ',i5,'  levels') 
171 format('zdef  1 linear 1.0 1.0') 
175 format(43i5)
180 format('tdef  1 linear 00z22jun2006 12hr   ')
190 format('vars  5')
200 format('rmean   ', i5, '    99    * mean')
210 format('rstd    ',i5,'     99     * standard deviation')
220 format('xmed    ',i5,'     99     * median value(o-b,o-a,smad(o-b),smad(o-a)')
230 format('break   ',i5,'     99     * median value(begining,end,smad(beeg.,smad(end)')
240 format('no      ',i5,'     99     *  number')
250 format('endvars')



close(11)

return
end
