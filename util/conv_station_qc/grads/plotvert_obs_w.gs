** this plot program plots the rms of 2 files for comparison 
function ploterr(args)

'reinit'

dtype=DTYPE
dtime=DTIME
stype=STYPE
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
   plotvert(dtype,stype,ix,nz,dtime,xize,ysize,debug)
ix=ix+1
endwhile

 function plotvert(dtype,stype,ix,nz,dtime,xize,ysize,debug)

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
'set z 1 'nz
v.1=rmean
*v.2=xmed
*v.3=break
if(stype = dir); iy=5;endif
if(stype = sp); iy=6;endif

max=0.0
min=10.0
'set gxout stat'
'set y 'iy
'd 'v.1
rec8=sublin(result,8)
max1=subwrd(rec8,5)
min1=subwrd(rec8,4)
say 'max1='max1
say 'min1='min1
if( max <= max1); max=max1;endif
if( min >= min1); min=min1;endif

*max=max+max/10
max=max+0.1
*min=min-min/5
min=min-0.1

*if(max >13.0);max=13.0;endif
say 'max='max
say 'min='min


'set gxout line'
'set cstyle 1' 
'set ccolor 1'
'set cmark 1'
'set axlim 'min' 'max
'set y 'iy 
'd rmean' 

'draw xlab  'dtype'_'var','dtime
'draw title  'stationid',Obs,lat:'lat


'set vpage 4 9 0 8.5'
'set grads off'
'set grid on'
'set xlopts 1 4 0.10'
'set ylopts 1 4 0.10'
'set x  'ix
'set z 1 'nz
if(stype = dir); iy=5;endif
if(stype = sp); iy=6;endif

max=0.0
min=10.0
'set gxout stat'
'set y 'iy
'd rstd'
rec8=sublin(result,8)
max1=subwrd(rec8,5)
min1=subwrd(rec8,4)
say 'max1='max1
if( max <= max1); max=max1;endif
if( min >= min1); min=min1;endif

max=max+0.1
min=min-0.1

'set gxout line'
'set cstyle 1'
'set ccolor 1' 
'set cmark 1'
'set axlim 'min' 'max
'set y 'iy 
'd rstd'


'draw xlab  st higt:'hgt',Std' 
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

*outfile=dtype'_'stype'_obs_'ix'_'stationid'_'ins'.png'
outfile=dtype'_'stype'_obs_'stationid'.png'


'printim 'outfile' x1000 y850 white'

if(debug=1)
say 'enter'
pull x
endif

return
