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
debug=1

say 'nx='nx
say 'nz='nz

** plot direction vertical profile 
ix=1
while(ix <=nx)
   plotvert(dtype,ix,nz,dir,dtime,xize,ysize,debug)
ix=ix+1
endwhile

** plot speed vertical profile
ix=1
while(ix <=nx)
   plotvert(dtype,ix,nz,sp,dtime,xize,ysize,debug)
ix=ix+1
endwhile

 function plotvert(dtype,ix,nz,var,dtime,xize,ysize,debug)

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
i=1
group=1
while(i <=group)
if( var = dir)
ny=2
iy=1
endif
if( var = sp )
ny=4
iy=3
endif
while (iy <= ny)
'set y 'iy
'd 'v.i
rec8=sublin(result,8)
max1=subwrd(rec8,5)
min1=subwrd(rec8,4)
say 'max1='max1
if( max <= max1); max=max1;endif
if( min >= min1); min=min1;endif

iy=iy+1
endwhile
i=i+1
endwhile

max=max+max/20
min=min-min/5

*if(max >13.0);max=13.0;endif
say 'max='max
say 'min='min

color.1=1
color.2=2
color.3=1
color.4=2

'set gxout line'
i=1
if( var = dir)
ny=2
iy=1
endif
if( var = sp)
ny=4
iy=3
endif
while(iy <=ny)
'set cstyle 1'
'set ccolor 'color.iy
'set cmark 1'
'set axlim 'min' 'max
'set y 'iy
'd rmean'
iy=iy+1
endwhile

'draw xlab  'dtype'_'var' 'dtime
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
if( var = dir)
'set y 1'
endif
if(  var =  sp)
'set y 3'
endif
'd rstd'
rec8=sublin(result,8)
max1=subwrd(rec8,5)
min1=subwrd(rec8,4)
say 'max1='max1
if( max <= max1); max=max1;endif
if( min >= min1); min=min1;endif
if( var = dir)
'set y 2'
endif
if( var = sp)
'set y 4'
endif
'd rstd'
rec8=sublin(result,8)
max1=subwrd(rec8,5)
min1=subwrd(rec8,4)
say 'max1='max1
if( max <= max1); max=max1;endif
if( min >= min1); min=min1;endif


'set gxout line'
'set cstyle 1'
'set ccolor 1' 
'set axlim 'min' 'max
if( var = dir)
'set y 1'
'set cmark 1'
endif
if( var = sp)
'set y 3'
'set cmark 1'
endif
'd rstd'
'set cstyle 1'
'set ccolor 2'
'set cmark 1'
'set axlim 'min' 'max
if( var = dir)
'set y 2'
endif
if( var = sp)
'set y 4'
endif
'd rstd'


'draw xlab  st higt:'hgt',RMS(o-b:blk)' 
'draw title  lon:'lon',ins. typ:'ins

*'set vpage 0 11 0 8.5'
*'run linesmpos.gs legrms 3.9 6.7 6.3 7.8'

'set vpage 8 11 0 8.5'
'set gxout line'
'set grads off'
*'set grid off'
'set x  'ix
'set y 1'
'set z 1 43'
'set cstyle 1'
'set ccolor 1' 
'set cmark 1' 
'd no/10'

'draw xlab  data no. in 10 '
*'draw ylab height' 

*outfile=dtype'_'var'_'ix'_'ins'_'stationid'.png'
outfile=dtype'_'var'_'stationid'.png'

'printim 'outfile' x1000 y850 white'

if(debug=1)
say 'enter'
pull x
endif

return
