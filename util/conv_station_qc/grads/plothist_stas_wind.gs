*  plot histogram  series

'reinit'

dtype=DTYPE
dtime=DTIME

'open 'dtype'_stas_hist.ctl'
'set grads off'
debug=0

'q file'
lin5=sublin(result,5)
nx=subwrd(lin5,3)
nz=subwrd(lin5,9)

say 'nx='nx
say 'nz='nz

'! rm -f sinfo.txt'
'! cat 'dtype'_stas_hist.ctl |grep  "interval" > sinfo.txt'
result=read(sinfo.txt)
rc=sublin(result,1)
factor1=5.0
factor2=0.2
say 'rc='rc
if (rc = 0)
info=sublin(result,2)
factor1=subwrd(info,4)
factor2=subwrd(info,5)
endif
*if (factor=0.1)
xrange1=nx*factor1
fact1=-factor1/2
xrange2=nx*factor2
fact2=-factor2/2
*endif
say 'xrange1='xrange1
say 'xrange2='xrange2


iz=1

while (iz <=nz)

plothist(iz,nx,nz,dir,dtype,xrange1,fact1,dtime,debug)
plothist(iz,nx,nz,sp,dtype,xrange2,fact2,dtime,debug)
iz=iz+1
endwhile

function plothist(iz,nx,nz,var,dtype,xrange,fact,dtime,debug)

levz.1=1000mb;levz.2=975mb;levz.3=950mb;levz.4=925mb;levz.5=900mb;levz.6=875mb
levz.7=850mb;levz.8=825mb;levz.9=800mb;levz.10=775mb;levz.11=750mb;levz.12=725mb
levz.13=700mb;levz.14=675mb;levz.15=650mb;levz.16=625mb;levz.17=600mb;levz.18=575mb
levz.19=550mb;levz.20=525mb;levz.21=500mb;levz.22=475mb;levz.23=450mb;levz.24=425mb
levz.25=400mb;levz.26=375mb;levz.27=350mb;levz.28=325mb;levz.29=300mb;levz.30=275mb
levz.31=250mb;levz.32=225mb;levz.33=200mb;levz.34=175mb;levz.35=150mb;levz.36=125mb
levz.37=100mb;levz.38=70mb;levz.39=50mb;levz.40=30mb;levz.41=20mb;levz.42=10mb;levz.43=5mb


if(var = dir)
var.1=rmd
var.2=rstd
else
var.1=rms
var.2=rsts
endif
nf=2
'clear'

say 'nz='nz
maxvar=-100.0
maxvar2=-100.0
'set x 1 'nx
'set y 1'
'set z 'iz
'set gxout stat'
'd 'var.1
rec8=sublin(result,8)
   max1=subwrd(rec8,5)
if(maxvar <max1);maxvar=max1;endif
say 'max1='max1
say 'maxvar='maxvar
maxvar=maxvar+maxvar/10.0

'set parea 1.0 8.0 6.0 10.0'
'set x 0 'nx
'set y 1'
'set z 'iz
'set gxout bar'
'set barbase 0'
'set ccolor 8'
'set xaxis  'fact' ' xrange
*'set xaxis  -0.05 ' xrange
'set vrange 0 'maxvar
'd 'var.1
ystring=10.1
'draw string '1.0' 'ystring' 'dtype'_'var',o-b abs mean No. historgram at height 'levz.iz','dtime

say 'nz='nz
maxvar=-100.0
maxvar2=-100.0
'set x 1 'nx
'set y 1'
'set z 'iz
'set gxout stat'
'd 'var.2
rec8=sublin(result,8)
   max1=subwrd(rec8,5)
if(maxvar <max1);maxvar=max1;endif
say 'max1='max1
say 'maxvar='maxvar
maxvar=maxvar+maxvar/10.0
'set parea 1.0 8.0 1.0 5.0'
'set x 0 'nx
'set y 1'
'set z 'iz
'set gxout bar'
'set barbase 0'
'set ccolor 8'
'set xaxis  'fact' ' xrange
*'set xaxis  -0.05 ' xrange
'set vrange 0 'maxvar
'd 'var.2
ystring=5.1
'draw string '1.0' 'ystring' 'dtype'_'var',o-b RMS No. historgram at height 'levz.iz','dtime



outfile=dtype'_'var'_lev'iz'.png'
'printim 'outfile' x1000 y850 white'
if(debug=1)
say 'enter'
pull x
endif

return
