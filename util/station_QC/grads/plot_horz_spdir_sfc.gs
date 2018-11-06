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

nhe=1
nz=1

if (dtype = uv280); nhe=9; nz=1;endif
if (dtype= uv287 | dtype= uv281); nhe=23; nz=1;endif

say 'dtype='dtype
say 'nhe='nhe
say 'nz='nz

she=1
while (she <=nhe)
say 'she='she
plot_horz(dtype,rsddirb,xsize,ysize,rdate,she,debug)
plot_horz(dtype,rsddira,xsize,ysize,rdate,she,debug)
plot_horz(dtype,rsdspdb,xsize,ysize,rdate,she,debug)
plot_horz(dtype,rsdspda,xsize,ysize,rdate,she,debug)
plot_horz(dtype,nob,xsize,ysize,rdate,she,debug)
plot_horz(dtype,no_rej,xsize,ysize,rdate,she,debug)
plot_horz(dtype,rmdirb,xsize,ysize,rdate,she,debug)
plot_horz(dtype,rmdira,xsize,ysize,rdate,she,debug)
plot_horz(dtype,rmspdb,xsize,ysize,rdate,she,debug)
plot_horz(dtype,rmspda,xsize,ysize,rdate,she,debug)
she=she+1
endwhile

function plot_horz(dtype,var,xsize,ysize,rdate,she,debug)

'rgbset2'
'set background 99'
'page'
'c'
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
if( var = nob | var = no_rej | var = rsddirb | var =rsddira )
'set dignum 0'
else
'set dignum 1'
endif
if(var = rsddirb )
'set ccolor 4'
'd maskout(rsddirb,60.0-rsddirb)'
'set ccolor 6'
'd maskout(rsddirb,rsddirb-60.0)'
'draw title 'dtype'_dir,RMS(O-B) at reg. 'he',avgd over 'rdate 
'printim 'dtype'_dir_rstdb_region'he'.png 'xsize' 'ysize
endif
if(var = rsddira )
'set ccolor 4'
'd maskout(rsddira,60.0-rsddira)'
'set ccolor 6'
'd maskout(rsddira,rsddira-60.0)'
'draw title 'dtype'_dir,RMS(O-A) at reg. 'he',avgd over 'rdate
'printim 'dtype'_dir_rstda_region'he'.png 'xsize' 'ysize
endif
if(var = rsdspdb )
'set ccolor 4'
'd maskout(rsdspdb,3.5-rsdspdb)'
'set ccolor 6'
'd maskout(rsdspdb,rsdspdb-3.5)'
'draw title 'dtype'_spd,RMS(O-B) at reg. 'he',avgd over 'rdate 
'printim 'dtype'_sp_rstdb_region'he'.png 'xsize' 'ysize
endif
if(var = rsdspda )
'set ccolor 4'
'd maskout(rsdspda,3.5-rsdspda)'
'set ccolor 6'
'd maskout(rsdspda,rsdspda-3.5)'
'draw title 'dtype'_spd,RMS(O-A) at reg. 'he',avgd over 'rdate 
'printim 'dtype'_sp_rstda_region'he'.png 'xsize' 'ysize
endif
if(var = nob)
'set ccolor 4'
'd nob'
'draw title 'dtype',data No. used at reg. 'he',avgd over 'rdate 
'printim 'dtype'_sp_nob_region'he'.png 'xsize' 'ysize
endif
if(var = no_rej)
'set ccolor 6'
'd no_rej'
'set ccolor 6'
'd no_rej'
'draw title 'dtype',data No. rjted at reg. 'he',avgd over 'rdate 
'printim 'dtype'_sp_no_rej_region'he'.png 'xsize' 'ysize
endif
if( var = rmdirb)
'set ccolor 2'
'd maskout(rmdirb,abs(rmdirb)-25.0)'
'set ccolor 4'
'd maskout(rmdirb,25.0-abs(rmdirb))'
'draw title 'dtype'_dir,Bias(O-B) at reg. 'he',avgd over 'rdate 
'printim 'dtype'_dir_rmeanb_region'he'.png 'xsize' 'ysize
endif
if(var = rmdira)
'set ccolor 2'
'd maskout(rmdira,abs(rmdira)-25.0)'
'set ccolor 4'
'd maskout(rmdira,25.0-abs(rmdira))'
'draw title 'dtype'_dir,Bias(O-A) at reg. 'he',avgd over 'rdate 
'printim 'dtype'_dir_rmeana_region'he'.png 'xsize' 'ysize
endif
if(var = rmspdb)
'set ccolor 2'
'd maskout(rmspdb,abs(rmspdb)-2.0)'
'set ccolor 4'
'd maskout(rmspdb,2.0-abs(rmspdb))'
'draw title 'dtype'_speed,Bias(O-B) at reg. 'he',avgd over 'rdate 
'printim 'dtype'_sp_rmeanb_region'he'.png 'xsize' 'ysize
endif
if(var = rmspda)
'set ccolor 2'
'd maskout(rmspda,abs(rmspda)-2.0)'
'set ccolor 4'
'd maskout(rmspda,2.0-abs(rmspda))'
'draw title 'dtype'_speed,Bias(O-A) at reg. 'he',avgd over 'rdate 
'printim 'dtype'_sp_rmeana_region'he'.png 'xsize' 'ysize
endif

*'printim 'dtype'_'var'_region'he'.png 'xsize' 'ysize
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
'set lat -90 90';'set lon 0 360'
endif
if(he =2)
if(plotfile = uv220); 'set lat 0 40';'set lon 0 80';endif
if(plotfile = uv221); 'set lat 0 90';'set lon 0 180';endif
if(plotfile = uv223); 'set lat 20 50';'set lon 240 300';endif
if(plotfile = uv224); 'set lat 0 65';'set lon 120 250';endif
if(plotfile = uv228); 'set lat 25 65';'set lon 120 200';endif
if(plotfile = uv229); 'set lat -5 30';'set lon 90 160';endif
if(plotfile = uv280 ); 'set lat 30 70'; 'set lon 0 75';endif
if(plotfile = uv281 | plotfile = uv287 ); 'set lat 0 35'; 'set lon 0 60' ;endif
endif
if(he = 3)
if(plotfile = uv220); 'set lat 30 75';'set lon 0 80';endif
if(plotfile = uv221); 'set lat 0 90';'set lon 180 360';endif
if(plotfile = uv224); 'set lat 20 50';'set lon 240 300';endif
if(plotfile = uv280 ); 'set lat -35 30'; 'set lon 0 120';endif
if(plotfile = uv281 | plotfile = uv287); 'set lat 35 50'; 'set lon 0 30' ;endif
endif
if(he = 4)
if(plotfile = uv220); 'set lat 0 40';'set lon 80 160';endif
if(plotfile = uv221); 'set lat -90 0';'set lon 0 180';endif
if(plotfile = uv280 ); 'set lat 0 60'; 'set lon 120 240';endif
if(plotfile = uv281 | plotfile = uv287 ); 'set lat 50 80'; 'set lon 0 60' ;endif
endif
if(he = 5)
if(plotfile = uv220); 'set lat 30 75';'set lon 80 160';endif
if(plotfile = uv221); 'set lat -90 0';'set lon 180 360';endif
if(plotfile = uv280 ); 'set lat 0 30'; 'set lon 240 300';endif
if(plotfile = uv281 | plotfile = uv287 ); 'set lat 35 50 '; 'set lon 30 60' ;endif
endif
if(he = 6)
if(plotfile = uv220);'set lat 0 75';'set lon 120 260 ';endif
if(plotfile = uv280 ); 'set lat 30 60'; 'set lon 240 300';endif
if(plotfile = uv281 | plotfile = uv287 ); 'set lat 0 35 '; 'set lon 60 120' ;endif
endif
if(he = 7)
if(plotfile = uv220); 'set lat 20 65';'set lon 240 320';endif
if(plotfile = uv280 ); 'set lat 0 32'; 'set lon 300 360';endif
if(plotfile = uv281 | plotfile = uv287 ); 'set lat 35 50'; 'set lon 60 90' ;endif

endif
if(he = 8)
if(plotfile = uv220); 'set lat 20 65';'set lon 280 360';endif
if(plotfile = uv280 ); 'set lat 32 65'; 'set lon 300 360';endif
if(plotfile = uv281 | plotfile = uv287 ); 'set lat 50 80'; 'set lon 60 120' ;endif
endif
if(he = 9)
if(plotfile = uv220); 'set lat -35 30';'set lon 0 120';endif
if(plotfile = uv280 ); 'set lat -60 0'; 'set lon 120 240';endif
if(plotfile = uv281 | plotfile = uv287 ); 'set lat 35 50'; 'set lon 90 120' ;endif
endif
if(he = 10)
if(plotfile = uv220); 'set lat -65 0';'set lon 120 240';endif
if(plotfile = uv280 ); 'set lat -60 0'; 'set lon 240 360';endif
if(plotfile = uv281 | plotfile = uv287 ); 'set lat 0 70'; 'set lon 120 240' ;endif
endif
if(he = 11)
if(plotfile = uv220); 'set lat -65 20';'set lon 200 360';endif
if(plotfile = uv281 | plotfile = uv287 );'set lat 0 35'; 'set lon 240 300' ;endif
endif
if(he = 12)
if(plotfile = uv281 | plotfile = uv287 ); 'set lat 35 50'; 'set lon 240 270';endif
endif
if(he = 13)
if(plotfile = uv281 | plotfile = uv287 ); 'set lat 50 80'; 'set lon 240 300';endif
endif
if(he = 14)
if(plotfile = uv281 | plotfile = uv287 ); 'set lat 35 50'; 'set lon 270 300';endif
endif
if(he = 15)
if(plotfile = uv281 | plotfile = uv287 ); 'set lat 0 30'; 'set lon 300 360';endif
endif
if(he = 16)
if(plotfile = uv281 | plotfile = uv287 ); 'set lat 30 60'; 'set lon 300 360';endif
endif
if(he = 17)
if(plotfile = uv281 | plotfile = uv287 ); 'set lat 60 90'; 'set lon 300 360';endif
endif
if(he = 18)
if(plotfile = uv281 | plotfile = uv287 ); 'set lat -60 0'; 'set lon 0 120';endif
endif
if(he = 19)
if(plotfile = uv281 | plotfile = uv287 ); 'set lat -30 0'; 'set lon 120 180';endif
endif
if(he = 20)
if(plotfile = uv281 | plotfile = uv287 ); 'set lat -90 -30'; 'set lon 120 240';endif
endif
if(he = 21)
if(plotfile = uv281 | plotfile = uv287 ); 'set lat -30 0'; 'set lon 240 300';endif
endif
if(he = 22)
if(plotfile = uv281 | plotfile = uv287 ); 'set lat -75 -30'; 'set lon 260 335';endif
endif
if(he = 23)
if(plotfile = uv281 | plotfile = uv287 ); 'set lat -30 0'; 'set lon 300 360';endif
endif

return

return
