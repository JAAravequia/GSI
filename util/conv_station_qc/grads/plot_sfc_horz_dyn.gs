
'reinit'

'rgbset2'
'set background 99'
'page'
'c'

dtype=DTYPE
rdate=RDATE
xsize=x1000
ysize=y850
'open 'dtype'_stas_station.ctl'
'set grads off'
debug=0

nz=1
nhe=1


she=1
while (she <=nhe)
say 'dtype='dtype
say 'nhe='nhe
plot_horz(dtype,rmeanb,xsize,ysize,rdate,she,debug)
plot_horz(dtype,rmeana,xsize,ysize,rdate,she,debug)
plot_horz(dtype,rstdb,xsize,ysize,rdate,she,debug)
plot_horz(dtype,rstda,xsize,ysize,rdate,she,debug)
plot_horz(dtype,nob,xsize,ysize,rdate,she,debug)
plot_horz(dtype,no_rej,xsize,ysize,rdate,she,debug)
she=she+1
endwhile

function plot_horz(dtype,var,xsize,ysize,rdate,she,debug)

'rgbset2'
'set background 99'
'page'
'c'
he=she
setmap(dtype,he)
'set digsiz 0.05'
if( var = nob | var = no_rej )
'set dignum 0'
else
'set dignum 1'
endif
if( var = rmeanb)
'set ccolor 6'
'd maskout(rmeanb,abs(rmeanb)-1.5)'
'set ccolor 4'
'd maskout(rmeanb,1.5-abs(rmeanb))'
'draw title 'dtype',Bias(O-B) at region 'he' avgd over 'rdate
endif
if( var = rmeana)
'set ccolor 6'
'd maskout(rmeana,abs(rmeana)-1.5)'
'set ccolor 4'
'd maskout(rmeana,1.5-abs(rmeana))'
'draw title 'dtype',Bias(O-A) at region 'he' avgd over 'rdate
endif
if( var = rstdb)
'set ccolor 6'
'd maskout(rstdb,rstdb-1.5)'
'set ccolor 4'
'd maskout(rstdb,1.5-rstdb)'
'draw title 'dtype',RMS(O-B) at region 'he' avgd over 'rdate
endif
if( var = rstda)
'set ccolor 6'
'd maskout(rstda,rstda-1.5)'
'set ccolor 4'
'd maskout(rstda,1.5-rstda)'
'draw title 'dtype',RMS(O-A)at region 'he' avgd over 'rdate
endif
if( var = nob  )
'set ccolor 4'
'd nob'
'draw title 'dtype',data No. used at region 'he' avgd over 'rdate
endif
if( var = no_rej  )
'set ccolor 6'
'd no_rej'
'draw title 'dtype',data No. rejected at region 'he' avgd over 'rdate
endif

'printim 'dtype'_'var'_region'he'.png 'xsize' 'ysize
if(debug=1)
say 'enter'
pull x
endif


return

function setmap(plotfile,he)
'set annot 98'
'set line 98'
'set map 98'
'set xlopts 98'
'set ylopts 98'
if(he = 1)
'set lat -30 30'; 'set lon 30 150'
endif
return

