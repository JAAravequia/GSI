*  plot time series

'reinit'

dtype=DTYPE
sdate=STDATE
'open 'dtype'_time.ctl'
'set grads off'
debug=0


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
'! rm -f stinfo.txt'
'! cat 'dtype'_time.ctl |grep  " 'ix' stationid=" > stinfo.txt'
result=read(stinfo.txt)
rc=sublin(result,1)
*say 'rc='rc
if (rc = 0)
  info=sublin(result,2)
  stationid=subwrd(info,5)
*  say ' 'stationid
  lon=subwrd(info,7)
  lat=subwrd(info,9)
  hgt=subwrd(info,11)
  styp=subwrd(info,13)
endif
result=close(stinfo.txt)

*plottime(ix,nz,stationid,lon,lat,hgt,styp,dtype,dir,debug)
plottime(ix,nz,stationid,lon,lat,hgt,styp,dtype,sp,sdate,debug)

ix=ix+1
endwhile

function plottime(ix,nz,stationid,lon,lat,hgt,styp,dtype,var,sdate,debug) 

iz=1
nf=4
while (iz <=nz)
*'page'
'clear'

levz.1=1000mb;levz.2=975mb;levz.3=950mb;levz.4=925mb;levz.5=900mb;levz.6=875mb
levz.7=850mb;levz.8=825mb;levz.9=800mb;levz.10=775mb;levz.11=750mb;levz.12=725mb
levz.13=700mb;levz.14=675mb;levz.15=650mb;levz.16=625mb;levz.17=600mb;levz.18=575mb
levz.19=550mb;levz.20=525mb;levz.21=500mb;levz.22=475mb;levz.23=450mb;levz.24=425mb
levz.25=400mb;levz.26=375mb;levz.27=350mb;levz.28=325mb;levz.29=300mb;levz.30=275mb
levz.31=250mb;levz.32=225mb;levz.33=200mb;levz.34=175mb;levz.35=150mb;levz.36=125mb
levz.37=100mb;levz.38=70mb;levz.39=50mb;levz.40=30mb;levz.41=20mb;levz.42=10mb;levz.43=5mb

if=1
rz=iz
if(iz=36);nf=3;endif
while(if <=nf)
y1=10.6-(if-1)*2.5
y2=y1-1.8
ystring=y1+0.1

maxvar=-100.0
minvar=100.0
max1=0.0
min1=0.0
if(rz >=nz);rz=nz;endif
'set x 'ix
'set y 1'
'set z 'rz
*'set t 1 last'
'set t 'sdate' last'
'set gxout stat'
if(var = 'dir') 
'd obg'
else
'd obgv'
endif
rec8=sublin(result,8)
   min1=subwrd(rec8,4)
   max1=subwrd(rec8,5)
*say 'min1='min1
*say 'max1='max1
*say 'rz='rz
*say 'if='if
if(maxvar <max1);maxvar=max1;endif
if(minvar >min1);minvar=min1;endif
if(var = 'dir')
'd oba'
else
'd obav'
endif
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

'set parea 1.0 8.0 'y2' 'y1
'set gxout line'
*'set t 1 last'
'set t 'sdate' last'
'set datawarn off'
'set tlsupp year'
'set grads off'
'set x 'ix
'set y 1'
'set z 'rz
'set vrange 'minvar' 'maxvar
*say 'if='if
if(rz >=nz);rz=nz;endif
*say 'iz='iz
*say 'rz='rz
*say 'if='if
'set z 'rz
*'set vrange 'minvar' 'maxvar
'set ccolor 1'
'set cmark 3'
'set cstyle 1'
if(var = 'dir')
'd obg'
else
'd obgv'
endif
*'set vrange 'minvar' 'maxvar
'set ccolor 2'
'set cstyle 2'
'set cmark 1'
if(var = 'dir')
'd oba'
else
'd obav'
endif
'draw string '1.0' 'ystring' 'dtype'('var'),stid('ix'):'stationid',lat:'lat',lon:'lon',st. height:'hgt',levs:'levz.rz 
rz=rz+1
if=if+1
endwhile

rrz=rz-1
*outfile=dtype'_'var'_'ix'_'stationid'_'levz.rz'.png'
outfile=dtype'_'var'_'stationid'_'levz.rrz'.png'
'printim 'outfile' x1000 y850 white'
if(debug=1)
say 'enter'
pull x
endif

iz=iz+3
endwhile


return
