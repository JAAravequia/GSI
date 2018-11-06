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
if(dtype = q120) 
nz=29
else
nz=43
endif

*if (dtype = t120); nhe=8;endif
nhe=6

say 'dtype='dtype
say 'nhe='nhe
say 'nz='nz

she=1
while (she <=nhe)
iz=1
while (iz <=nz)
plot_horz(dtype,rmeanb,xsize,ysize,rdate,she,iz,debug)
plot_horz(dtype,rmeana,xsize,ysize,rdate,she,iz,debug)
plot_horz(dtype,rstdb,xsize,ysize,rdate,she,iz,debug)
plot_horz(dtype,rstda,xsize,ysize,rdate,she,iz,debug)
plot_horz(dtype,nob,xsize,ysize,rdate,she,iz,debug)
plot_horz(dtype,no_rej,xsize,ysize,rdate,she,iz,debug)
iz=iz+1
endwhile
she=she+1
endwhile

function plot_horz(dtype,var,xsize,ysize,rdate,she,iz,debug)

'rgbset2'
'set background 99'
'page'
'c'
*if(var = rmean)
*var1=rmeanb
*var2=rmeana
*endif
*if(var = rstd)
*var1=rstdb
*var2=rstda
*endif
*if(var = no)
*var1=nob
*var2=no_rej
*endif
levz.1=1000;levz.2=975;levz.3=950;levz.4=925;levz.5=900;levz.6=875
levz.7=850;levz.8=825;levz.9=800;levz.10=775;levz.11=750;levz.12=725
levz.13=700;levz.14=675;levz.15=650;levz.16=625;levz.17=600;levz.18=575
levz.19=550;levz.20=525;levz.21=500;levz.22=475;levz.23=450;levz.24=425
levz.25=400;levz.26=375;levz.27=350;levz.28=325;levz.29=300;levz.30=275
levz.31=250;levz.32=225;levz.33=200;levz.34=175;levz.35=150;levz.36=125
levz.37=100;levz.38=70;levz.39=50;levz.40=30;levz.41=20;levz.42=10;levz.43=5

he=she
setmap(dtype,he)
'set digsiz 0.05'
if( var = nob | var = no_rej )
'set dignum 0'
else
'set dignum 1'
endif
'set z 'levz.iz
if( var = rmeanb)
'set ccolor 6'
'd maskout(rmeanb,abs(rmeanb)-1.5)'
'set ccolor 4'
'd maskout(rmeanb,1.5-abs(rmeanb))'
'draw title 'dtype',Bias(O-B) at reg. 'he','levz.iz'mb avgd over 'rdate
endif
if( var = rmeana)
'set ccolor 6'
'd maskout(rmeana,abs(rmeana)-1.5)'
'set ccolor 4'
'd maskout(rmeana,1.5-abs(rmeana))'
'draw title 'dtype',Bias(O-A) at reg 'he','levz.iz'mb avgd over 'rdate
endif
if( var = rstdb)
'set ccolor 6'
'd maskout(rstdb,rstdb-1.5)'
'set ccolor 4'
'd maskout(rstdb,1.5-rstdb)'
'draw title 'dtype',RMS(O-B) at reg 'he','levz.iz'mb avgd over 'rdate
endif
if( var = rstda)
'set ccolor 6'
'd maskout(rstda,rstda-1.5)'
'set ccolor 4'
'd maskout(rstda,1.5-rstda)'
'draw title 'dtype',RMS(O-A)at reg 'he','levz.iz'mb avgd over 'rdate
endif
if( var = nob  )
'set ccolor 4'
'd nob'
'draw title 'dtype',data No. used at reg 'he','levz.iz'mb avgd over 'rdate
endif
if( var = no_rej  )
'set ccolor 6'
'd no_rej'
'draw title 'dtype',data No. rejected at reg 'he','levz.iz'mb avgd over 'rdate
endif

'printim 'dtype'_'var'_region'he'_lev'levz.iz'.png 'xsize' 'ysize
return

function setmap(plotfile,he)
'set annot 98'
'set line 98'
'set map 98'
'set xlopts 98'
'set ylopts 98'
if(he = 1)
 'set lat -90 90';'set lon 0 360'
endif
if(he = 2)
*if(plotfile = ps120); 'set lat 30 75';'set lon 0 80';endif
 'set lat 30 75';'set lon 0 80'
endif
if(he = 3)
*if(plotfile = ps120); 'set lat 0 75';'set lon 120 260';endif
'set lat 0 75';'set lon 120 260'
endif
if(he = 4)
*if(plotfile = ps120); 'set lat 20 65';'set lon 240 320';endif
'set lat 20 65';'set lon 240 320'
endif
if(he = 5)
*if(plotfile = ps120); 'set lat 20 65';'set lon 280 360';endif
'set lat 20 65';'set lon 280 360'
endif
if(he = 6)
*if(plotfile = ps120); 'set lat -65 20';'set lon 200 360';endif
'set lat -65 20';'set lon 200 360'
endif

return

