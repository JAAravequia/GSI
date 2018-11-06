*  plot time series

'reinit'

dtype=DTYPE
sdate=STDATE
'open 'dtype'_time.ctl'
'set grads off'
debug=0


'set t 1 last'
'q time'
dmy=sublin(result,1)
ti=subwrd(dmy,5)
say ti
hh=substr(ti,1,2)
dd=substr(ti,4,2)

'q file'
lin5=sublin(result,5)
nx=subwrd(lin5,3)
nz=subwrd(lin5,9)

say 'nx='nx
say 'nz='nz

ix=1

while(ix <=nx)
plottime(ix,nx,dtype,sdate,debug)
ix=ix+1
*say 'ix='ix
endwhile

*ix=1

*while(ix <=nx)
*plottime(ix,nx,dtype,sp,sdate,debug)
*ix=ix+4
*endwhile


function plottime(ix,nx,dtype,sdate,debug) 

'clear'

 '! rm -f stinfo.txt'
'! cat 'dtype'_time.ctl |grep  " 'ix' dsid=" > stinfo.txt'
result=read(stinfo.txt)
rc=sublin(result,1)
*say 'rc='rc
if (rc = 0)
  info=sublin(result,2)
  stationid=subwrd(info,5)
*  say ' 'stationid
  lon=subwrd(info,7)
  lat=subwrd(info,9)
  sht=subwrd(info,11)
  obgd=subwrd(info,13)
  rmsd=subwrd(info,15)
  obsd=subwrd(info,17)
  stdd=subwrd(info,19)
  med=subwrd(info,21)
  smad=subwrd(info,23)
endif
result=close(stinfo.txt)
'! rm -f stinfo.txt'
'! cat 'dtype'_time.ctl |grep  " 'ix' ssid=" > stinfo.txt'
result=read(stinfo.txt)
rc=sublin(result,1)
*say 'rc='rc
if (rc = 0)
  info=sublin(result,2)
  obgs=subwrd(info,13)
  rmss=subwrd(info,15)
  obss=subwrd(info,17)
  stds=subwrd(info,19)
endif
result=close(stinfo.txt)
'! rm -f stinfo.txt'
*say 'obgd='obgd
*say 'rmsd='rmsd
*say 'obsd='obsd
*say 'stdd='stdd
*say 'med='med
*say 'smad='smad
*say 'obgs='obgs
*say 'rmss='rmss
*say 'obss='obss
*say 'stds='stds



   plotfiger(ix,dtype,sdate,10.6,dir,obg,oba,stationid,lat,lon,obgd,rmsd,med,smad,sht)
   plotfiger(ix,dtype,sdate,8.1,dir,obd,'obd-obg',stationid,lat,lon,obsd,stdd,med,smad,sht)
   plotfiger(ix,dtype,sdate,5.6,spd,obgv,obav,stationid,lat,lon,obgs,rmss,med,smad,sht)
   plotfiger(ix,dtype,sdate,3.1,spd,obs,'obs-obgv',stationid,lat,lon,obss,stds,med,smad,sht)

  outfile=dtype'_'stationid'.png'
'printim 'outfile' x1000 y850 white'
if(debug=1)
say 'enter'
pull x
endif

return

function plotfiger(ix,dtype,sdate,y1,var,var1,var2,stationid,lat,lon,ob,rms,med,smad,sht)
*say 'ix='ix
y2=y1-1.8
ystring=y1+0.1
maxvar=-100.0
minvar=2000.0
'set x 'ix
'set y 1'
'set z 1'
*'set t 1 last'
'set t 'sdate' last'
'set gxout stat'
'd 'var1

rec8=sublin(result,8)
   min1=subwrd(rec8,4)
   max1=subwrd(rec8,5)
*say 'min1='min1
*say 'max1='max1
*say 'ix='ix
*say 'if='if
if(maxvar <max1);maxvar=max1;endif
if(minvar >min1);minvar=min1;endif
'd ' var2
rec8=sublin(result,8)
   min1=subwrd(rec8,4)
   max1=subwrd(rec8,5)
*say 'min1='min1
*say 'max1='max1
if(maxvar <max1);maxvar=max1;endif
if(minvar >min1);minvar=min1;endif

if(minvar <0); minvar1=-minvar;endif
if(minvar >=0); minvar1=minvar;endif
maxvar=maxvar+maxvar/1000.0
minvar=minvar-minvar1/1000.0
*say 'maxvar='maxvar
*say 'minvar='minvar
*say 'if='if

'set parea 1.0 8.0 'y2' 'y1
'set gxout line'
*'set t 1 last'
'set t 'sdate' last'
'set datawarn off'
'set tlsupp year'
'set grads off'
'set x 'ix
'set y 1'
'set z 1'
'set vrange 'minvar' 'maxvar
'set ccolor 1'
'set cmark 3'
'set cstyle 1'
'd 'var1
'set ccolor 2' 
'set cstyle 2'
'set cmark 1'
'd 'var2
if ( var = dir  & var1 = obg )  
'draw string '1.0' 'ystring' 'dtype'('var'),id('ix'):'stationid',station height:'sht',o-b(b):'ob',std:'rms
endif
if ( var = dir  & var1 = obd )  
'draw string '1.0' 'ystring' 'dtype'('var'),lat:'lat',lon:'lon',obs(mean):'ob',std:'rms
endif
if ( var = spd  & var1 = obgv )  
'draw string '1.0' 'ystring' 'dtype'('var'),id('ix'):'stationid',o-b(b):'ob',std:'rms
endif
if ( var = spd  & var1 = obs )  
'draw string '1.0' 'ystring' 'dtype'('var'),lat:'lat',lon:'lon',obs(mean):'ob',std:'rms
endif
return
