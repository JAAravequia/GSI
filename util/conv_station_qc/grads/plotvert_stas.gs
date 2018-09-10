** this plot program plots the rms of 2 files for comparison 
function ploterr(args)

'reinit'

dtype=DTYPE
dtime=DTIME
xsize=x1000
ysize=y850
'open 'dtype'_stas.ctl'

'q file'
lin5=sublin(result,5)
nx=subwrd(lin5,3)
nz=subwrd(lin5,9)
debug=0

say 'nx='nx
say 'nz='nz

ix=1
while(ix <=nx)
   plotvert(dtype,ix,nz,dtime,xize,ysize,debug)
ix=ix+1
endwhile

 function plotvert(dtype,ix,nz,dtime,xize,ysize,debug)

'! rm -f stinfo.txt'
'! cat 'dtype'_stas.ctl |grep  " 'ix' stationid=" > stinfo.txt'
result=read(stinfo.txt)
rc=sublin(result,1)
say 'rc='rc
if (rc = 0)
  info=sublin(result,2)
  stationid=subwrd(info,5)
  say ' 'stationid
  lon=subwrd(info,7)
  lat=subwrd(info,9)
  hgt=subwrd(info,11)
  ins=subwrd(info,15)
endif
result=close(stinfo.txt)

'reset'
'set vpage 0 11 0 8.5'
'set strsiz 0.08'
*'set string 4 tl 6'
*'draw string 0.12 0.12  GMB/EMC/NCEP/NWS/NOAA'

'set vpage 0 5 0 8.5'
'set grads off'
'set grid on'
'set xlopts 1 4 0.10'
'set ylopts 1 4 0.10' 
'set x  'ix
*'set y 1'
'set z 1 'nz
v.1=rmean
*v.2=xmed
*v.3=break

max=0.0
min=10.0
'set gxout stat'
ny=2
iy=1
while (iy <= ny)
'set y 'iy
'd rmean' 
rec8=sublin(result,8)
max1=subwrd(rec8,5)
min1=subwrd(rec8,4)
say 'max1='max1
say 'min1='min1
if( max <= max1); max=max1;endif
if( min >= min1); min=min1;endif

iy=iy+1
endwhile

*max=max+max/10
max=max+0.1
*min=min-min/5
min=min-0.1

*if(max >13.0);max=13.0;endif
say 'max='max
say 'min='min

color.1=1
color.2=2

'set gxout line'
ny=2
iy=1
while(iy <=ny)
'set cstyle 1' 
'set ccolor ' color.iy 
'set cmark 1'
'set axlim 'min' 'max
'set y 'iy
'd rmean' 
iy=iy+1
endwhile

'draw xlab  'dtype' 'dtime
'draw title  'stationid',Bias,lat:'lat


'set vpage 4 9 0 8.5'
'set grads off'
'set grid on'
'set xlopts 1 4 0.10'
'set ylopts 1 4 0.10'
'set x  'ix
*'set y 1'
'set z 1 'nz

max=0.0
min=10.0
'set gxout stat'
'set y 1'
'd rstd'
rec8=sublin(result,8)
max1=subwrd(rec8,5)
min1=subwrd(rec8,4)
say 'max1='max1
if( max <= max1); max=max1;endif
if( min >= min1); min=min1;endif
'set y 2'
'd rstd'
rec8=sublin(result,8)
max1=subwrd(rec8,5)
min1=subwrd(rec8,4)
say 'max1='max1
if( max <= max1); max=max1;endif
if( min >= min1); min=min1;endif

max=max+0.1
min=min-0.1


color.1=2
ccstyle.3=1
ccstyle.4=3

'set gxout line'
'set cstyle 1'
'set ccolor 1' 
'set cmark 1'
'set axlim 'min' 'max
'set y 1' 
'd rstd'
'set cstyle 1'
'set ccolor 2'
'set cmark 1'
'set axlim 'min' 'max
'set y 2'
'd rstd'


'draw xlab  st higt:'hgt',RMS(o-b,blk)' 
'draw title  lon:'lon',ins. typ:'ins

*'set vpage 0 11 0 8.5'
*'run linesmpos.gs legrms 3.9 6.7 6.3 7.8'

'set vpage 8 11 0 8.5'
'set gxout line'
'set grads off'
*'set grid off'
'set x  'ix
'set y 1'
'set z 1 'nz
'set cstyle 1'
'set ccolor 1' 
'set cmark 1' 
'd no/10'

'draw xlab  data no. in tens '
*'draw ylab height' 

*outfile=dtype'_'ix'_'stationid'_'ins'.png'
outfile=dtype'_'stationid'.png'

'printim 'outfile' x1000 y850 white'

if(debug=1)
say 'enter'
pull x
endif

return
