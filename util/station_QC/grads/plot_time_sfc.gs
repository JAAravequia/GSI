*  plot time series

'reinit'

dtype=DTYPE
sdate=STDATE
'open 'dtype'_time.ctl'
'set grads off'
debug=0


*'set t 1 last'
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
*'! rm -f stinfo.txt'
*'! cat 'dtype'_time.ctl |grep  " 'ix' stationid=" > stinfo.txt'
*result=read(stinfo.txt)
*rc=sublin(result,1)
*say 'rc='rc
*if (rc = 0)
*  info=sublin(result,2)
*  stationid=subwrd(info,5)
*  say ' 'stationid
*  lon=subwrd(info,7)
*  lat=subwrd(info,9)
*  mean=subwrd(info,11)
*  std=subwrd(info,13)
*  med=subwrd(info,15)
*  smad=subwrd(info,15)
*endif
*result=close(stinfo.txt)

plottime(ix,nx,dtype,sdate,debug)

ix=ix+1
endwhile

function plottime(ix,nx,dtype,sdate,debug) 

'clear'

'! rm -f stinfo.txt'
'! cat 'dtype'_time.ctl |grep  " 'ix' sid=" > stinfo.txt'
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
  mean=subwrd(info,13)
  std=subwrd(info,15)
  obs=subwrd(info,17)
  obsrms=subwrd(info,19)
  med=subwrd(info,23)
  smad=subwrd(info,21)
endif
result=close(stinfo.txt)

maxvar=-100.0
minvar=100.0
'set x 'ix
'set y 1'
'set z 1'
*'set t 1 last'
'set t 'sdate' last'
'set gxout stat'
'd obg'
rec8=sublin(result,8)
   min1=subwrd(rec8,4)
   max1=subwrd(rec8,5)
*say 'min1='min1
*say 'max1='max1
*say 'rx='rx
*say 'if='if
if(maxvar <max1);maxvar=max1;endif
if(minvar >min1);minvar=min1;endif
'd oba'
rec8=sublin(result,8)
   min1=subwrd(rec8,4)
   max1=subwrd(rec8,5)
*say 'min1='min1
*say 'max1='max1
if(maxvar <max1);maxvar=max1;endif
if(minvar >min1);minvar=min1;endif

if(minvar <0); minvar1=-minvar;endif
if(minvar >=0); minvar1=minvar;endif
maxvar=maxvar+maxvar/10.0
minvar=minvar-minvar1/10.0
*say 'maxvar='maxvar
*say 'minvar='minvar
*say 'if='if


'set parea 1.0 8.0 6.0 10.0' 
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
*say 'if='if
*'set vrange 'minvar' 'maxvar
'set ccolor 1'
'set cmark 3'
'set cstyle 1'
'd obg' 
'set ccolor 3'
'd obg*0'
*'set vrange 'minvar' 'maxvar
'set ccolor 2' 
'set cstyle 2'
'set cmark 1'
'd oba'
'draw string 1.0 10.1 'dtype',id('ix'):'stationid',lat:'lat',lon:'lon',o-b(b):'mean',std:'std',med:'med',smad:'smad

maxvar=-2000.0
minvar=2000.0
'set x 'ix
'set y 1'
'set z 1'
*'set t 1 last'
'set t 'sdate' last'
'set gxout stat'
'd obs'
rec8=sublin(result,8)
   min1=subwrd(rec8,4)
   max1=subwrd(rec8,5)
*say 'min1='min1
*say 'max1='max1
*say 'rx='rx
*say 'if='if
if(maxvar <max1);maxvar=max1;endif
if(minvar >min1);minvar=min1;endif
'd obs-obg'
rec8=sublin(result,8)
   min1=subwrd(rec8,4)
   max1=subwrd(rec8,5)
*say 'min1='min1
*say 'max1='max1
if(maxvar <max1);maxvar=max1;endif
if(minvar >min1);minvar=min1;endif

if(minvar <0); minvar1=-minvar;endif
if(minvar >=0); minvar1=minvar;endif
maxvar=maxvar+maxvar/100.0
minvar=minvar-minvar1/100.0
*say 'maxvar='maxvar
*say 'minvar='minvar
*say 'if='if

'set parea 1.0 8.0 1.0 5.0'
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
*say 'if='if
'set ccolor 1'
'set cmark 3'
'set cstyle 1'
'd obs'
'set ccolor 2'
'set cstyle 2'
'set cmark 1'
'd obs-obg'
'draw string 1.0 5.1 'stationid',station height:'sht',obs(mean):'obs',std(obs):'obsrms

outfile=dtype'_'stationid'.png'
'printim 'outfile' x1000 y850 white'
if(debug=1)
say 'enter'
pull x
endif

