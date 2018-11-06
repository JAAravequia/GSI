
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
debug=1

nz=1
nhe=1
if (dtype = ps180 | dtype = q180 | dtype = t180); nhe=11;endif
if (dtype = ps183 ); nhe=9;endif
if (dtype = ps120 ); nhe=9;endif
if (dtype = ps187 | dtype = ps181 | dtype = q181 | dtype = q187 | dtype = t181 | dtype = t187); nhe=23;endif


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
'set lat -90 90'; 'set lon 0 360'
endif
if(he = 2)
if(plotfile = ps120); 'set lat 30 75';'set lon 0 80';endif
if(plotfile = ps180 | plotfile = t180 | plotfile = q180); 'set lat 30 70'; 'set lon 0 75';endif
if(plotfile = ps181 | plotfile = ps187 | plotfile = q181 | plotfile = q187 | plotfile = t181 | plotfile = t187); 'set lat 0 35'; 'set lon 0 60' ;endif
if(plotfile = ps183); 'set lat 0 35'; 'set lon 0 70';endif
endif
if(he = 3)
if(plotfile = ps120); 'set lat 30 75';'set lon 80 160';endif
if(plotfile = ps180 | plotfile = t180 | plotfile = q180); 'set lat -35 30'; 'set lon 0 120';endif
if(plotfile = ps181 | plotfile = ps187); 'set lat 35 50'; 'set lon 0 30' ;endif
if(plotfile = ps183); 'set lat 35 70'; 'set lon 0 70';endif
endif
if(he = 4)
if(plotfile = ps120);'set lat 0 75';'set lon 120 260 ';endif
if(plotfile = ps180 | plotfile = t180 | plotfile = q180); 'set lat 0 60'; 'set lon 120 240';endif
if(plotfile = ps181 | plotfile = ps187 | plotfile = q181 | plotfile = q187 | plotfile = t181 | plotfile = t187); 'set lat 50 80'; 'set lon 0 60' ;endif
if(plotfile = ps183); 'set lat 0 35'; 'set lon 70 130';endif
endif
if(he = 5)
if(plotfile = ps120); 'set lat 20 65';'set lon 240 320';endif
if(plotfile = ps180 | plotfile = t180 | plotfile = q180); 'set lat 0 30'; 'set lon 240 300';endif
if(plotfile = ps181 | plotfile = ps187 | plotfile = q181 | plotfile = q187 | plotfile = t181 | plotfile = t187); 'set lat 35 50 '; 'set lon 30 60' ;endif
if(plotfile = ps183);'set lat 35 70'; 'set lon 75 135';endif
endif
if(he = 6)
if(plotfile = ps120); 'set lat 20 65';'set lon 280 360';endif
if(plotfile = ps180 | plotfile = t180 | plotfile = q180); 'set lat 30 60'; 'set lon 240 300';endif
if(plotfile = ps181 | plotfile = ps187 | plotfile = q181 | plotfile = q187 | plotfile = t181 | plotfile = t187); 'set lat 0 35 '; 'set lon 60 120' ;endif
if(plotfile = ps183);'set lat 35 75'; 'set lon 220 300';endif
*if(plotfile = ps183);'set lat 0 40'; 'set lon 200 280';endif
endif
if(he = 7)
if(plotfile = ps120); 'set lat -35 30';'set lon 0 120';endif
if(plotfile = ps180 | plotfile = t180 | plotfile = q180); 'set lat 0 32'; 'set lon 300 360';endif
if(plotfile = ps181 | plotfile = ps187 | plotfile = q181 | plotfile = q187 | plotfile = t181 | plotfile = t187); 'set lat 35 50'; 'set lon 60 90' ;endif
if(plotfile = ps183);'set lat 30 65'; 'set lon 290 360';endif
endif
if(he = 8)
if(plotfile = ps120); 'set lat -65 0';'set lon 120 240';endif
if(plotfile = ps180 | plotfile = t180 | plotfile = q180); 'set lat 32 65'; 'set lon 300 360';endif
if(plotfile = ps181 | plotfile = ps187 | plotfile = q181 | plotfile = q187 | plotfile = t181 | plotfile = t187); 'set lat 50 80'; 'set lon 60 120' ;endif
if(plotfile = ps183);'set lat -90 0'; 'set lon 0 180';endif
endif
if(he = 9)
if(plotfile = ps120); 'set lat -65 20';'set lon 200 360';endif
if(plotfile = ps180 | plotfile = t180 | plotfile = q180); 'set lat -60 0'; 'set lon 120 240';endif
if(plotfile = ps181 | plotfile = ps187 | plotfile = q181 | plotfile = q187 | plotfile = t181 | plotfile = t187); 'set lat 35 50'; 'set lon 90 120' ;endif
if(plotfile = ps183);'set lat -90 0'; 'set lon 180 360';endif
endif
if(he = 10)
if(plotfile = ps180 | plotfile = t180 | plotfile = q180); 'set lat -60 0'; 'set lon 240 360';endif
if(plotfile = ps181 | plotfile = ps187 | plotfile = q181 | plotfile = q187 | plotfile = t181 | plotfile = t187); 'set lat 0 70'; 'set lon 120 240' ;endif
endif
if(he = 11)
if(plotfile = ps181 | plotfile = ps187 | plotfile = q181 | plotfile = q187 | plotfile = t181 | plotfile = t187);'set lat 0 35'; 'set lon 240 300' ;endif
if(plotfile = ps180 | plotfile = t180 | plotfile = q180); 'set lat -65 -5'; 'set lon 0 120';endif
endif
if(he = 12)
if(plotfile = ps181 | plotfile = ps187 | plotfile = q181 | plotfile = q187 | plotfile = t181 | plotfile = t187); 'set lat 35 50'; 'set lon 240 270';endif
endif
if(he = 13)
if(plotfile = ps181 | plotfile = ps187 | plotfile = q181 | plotfile = q187 | plotfile = t181 | plotfile = t187); 'set lat 50 80'; 'set lon 240 300';endif
endif
if(he = 14)
if(plotfile = ps181 | plotfile = ps187 | plotfile = q181 | plotfile = q187 | plotfile = t181 | plotfile = t187); 'set lat 35 50'; 'set lon 270 300';endif
endif
if(he = 15)
if(plotfile = ps181 | plotfile = ps187 | plotfile = q181 | plotfile = q187 | plotfile = t181 | plotfile = t187); 'set lat 0 30'; 'set lon 300 360';endif
endif
if(he = 16)
if(plotfile = ps181 | plotfile = ps187 | plotfile = q181 | plotfile = q187 | plotfile = t181 | plotfile = t187); 'set lat 30 60'; 'set lon 300 360';endif
endif
if(he = 17)
if(plotfile = ps181 | plotfile = ps187 | plotfile = q181 | plotfile = q187 | plotfile = t181 | plotfile = t187); 'set lat 60 90'; 'set lon 300 360';endif
endif
if(he = 18)
if(plotfile = ps181 | plotfile = ps187 | plotfile = q181 | plotfile = q187 | plotfile = t181 | plotfile = t187); 'set lat -60 0'; 'set lon 0 120';endif
endif
if(he = 19)
if(plotfile = ps181 | plotfile = ps187 | plotfile = q181 | plotfile = q187 | plotfile = t181 | plotfile = t187); 'set lat -30 0'; 'set lon 120 180';endif
endif
if(he = 20)
if(plotfile = ps181 | plotfile = ps187 | plotfile = q181 | plotfile = q187 | plotfile = t181 | plotfile = t187); 'set lat -90 -30'; 'set lon 120 240';endif
endif
if(he = 21)
if(plotfile = ps181 | plotfile = ps187 | plotfile = q181 | plotfile = q187 | plotfile = t181 | plotfile = t187); 'set lat -30 0'; 'set lon 240 300';endif
endif
if(he = 22)
if(plotfile = ps181 | plotfile = ps187 | plotfile = q181 | plotfile = q187 | plotfile = t181 | plotfile = t187); 'set lat -75 -30'; 'set lon 260 335';endif
endif
if(he = 23)
if(plotfile = ps181 | plotfile = ps187 | plotfile = q181 | plotfile = q187 | plotfile = t181 | plotfile = t187); 'set lat -30 0'; 'set lon 300 360';endif
endif
return

