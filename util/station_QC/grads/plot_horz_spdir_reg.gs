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

if (dtype =uv220); nhe=6;nz=43;endif
if (dtype =uv221); nhe=4; nz=43;endif
if (dtype =uv223); nhe=1; nz=43;endif
if (dtype =uv224); nhe=3; nz=43;endif
if (dtype =uv228); nhe=1; nz=43;endif
if (dtype =uv229); nhe=1; nz=43;endif
if (dtype =uv280); nhe=7; nz=1;endif
if (dtype =uv281 | dtype= uv287); nhe=13; nz=1;endif
if (dtype =uv288 ); nhe=76; nz=1;endif

say 'dtype='dtype
say 'nhe='nhe
say 'nz='nz

she=1
while (she <=nhe)
iz=1
say 'iz='iz
while (iz <=nz)
plot_horz(dtype,rsddirb,xsize,ysize,rdate,she,iz,debug)
plot_horz(dtype,rsddira,xsize,ysize,rdate,she,iz,debug)
plot_horz(dtype,rsdspdb,xsize,ysize,rdate,she,iz,debug)
plot_horz(dtype,rsdspda,xsize,ysize,rdate,she,iz,debug)
plot_horz(dtype,nob,xsize,ysize,rdate,she,iz,debug)
plot_horz(dtype,no_rej,xsize,ysize,rdate,she,iz,debug)
plot_horz(dtype,rmdirb,xsize,ysize,rdate,she,iz,debug)
plot_horz(dtype,rmdira,xsize,ysize,rdate,she,iz,debug)
plot_horz(dtype,rmspdb,xsize,ysize,rdate,she,iz,debug)
plot_horz(dtype,rmspda,xsize,ysize,rdate,she,iz,debug)
say 'iz='iz
say 'she='she
iz=iz+1
endwhile
she=she+1
endwhile

function plot_horz(dtype,var,xsize,ysize,rdate,she,iz,debug)

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
'set z 'levz.iz
if(var = rsddirb )
'set ccolor 4'
'd maskout(rsddirb,60.0-rsddirb)'
'set ccolor 6'
'd maskout(rsddirb,rsddirb-60.0)'
'draw title 'dtype'_dir,RMS(O-B) at reg. 'he','levz.iz'mb avgd over 'rdate 
'printim 'dtype'_dir_rstdb_region'he'_lev'levz.iz'.png 'xsize' 'ysize
endif
if(var = rsddira )
'set ccolor 4'
'd maskout(rsddira,60.0-rsddira)'
'set ccolor 6'
'd maskout(rsddira,rsddira-60.0)'
'draw title 'dtype'_dir,RMS(O-A) at reg. 'he','levz.iz'mb avgd over 'rdate 
'printim 'dtype'_dir_rstda_region'he'_lev'levz.iz'.png 'xsize' 'ysize
say 'level='levz.iz
say 'iz='iz
say 'debug='debug
endif
if(var = rsdspdb )
'set ccolor 4'
'd maskout(rsdspdb,3.5-rsdspdb)'
'set ccolor 6'
'd maskout(rsdspdb,rsdspdb-3.5)'
'draw title 'dtype'_spd,RMS(O-B) at reg. 'he','levz.iz'mb avgd over 'rdate 
'printim 'dtype'_sp_rstdb_region'he'_lev'levz.iz'.png 'xsize' 'ysize
endif
if(var = rsdspda )
'set ccolor 4'
'd maskout(rsdspda,3.5-rsdspda)'
'set ccolor 6'
'd maskout(rsdspda,rsdspda-3.5)'
'draw title 'dtype'_spd,RMS(O-A) at reg. 'he','levz.iz'mb avgd over 'rdate 
'printim 'dtype'_sp_rstda_region'he'_lev'levz.iz'.png 'xsize' 'ysize
endif
if(var = nob)
'set ccolor 4'
'd nob'
'draw title 'dtype',data No. used at reg. 'he','levz.iz'mb avgd over 'rdate 
'printim 'dtype'_sp_nob_region'he'_lev'levz.iz'.png 'xsize' 'ysize
endif
if(var = no_rej)
'set ccolor 6'
'd no_rej'
'draw title 'dtype',data No. rjted at reg. 'he','levz.iz'mb avgd over 'rdate 
'printim 'dtype'_sp_no_rej_region'he'_lev'levz.iz'.png 'xsize' 'ysize
endif
if( var = rmdirb)
'set ccolor 2'
'd maskout(rmdirb,abs(rmdirb)-25.0)'
'set ccolor 4'
'd maskout(rmdirb,15.0-abs(rmdirb))'
'draw title 'dtype'_dir,Bias(O-B) at reg. 'he','levz.iz'mb avgd over 'rdate 
'printim 'dtype'_dir_rmeanb_region'he'_lev'levz.iz'.png 'xsize' 'ysize
endif
if(var = rmdira)
'set ccolor 2'
'd maskout(rmdira,abs(rmdirb)-25.0)'
'set ccolor 4'
'd maskout(rmdira,25.0-abs(rmdirb))'
'draw title 'dtype'_dir,Bias(O-A) at reg. 'he','levz.iz'mb avgd over 'rdate 
'printim 'dtype'_dir_rmeana_region'he'_lev'levz.iz'.png 'xsize' 'ysize
endif
if(var = rmspdb) 
'set ccolor 2'
'd maskout(rmspdb,abs(rmspdb)-2.0)'
'set ccolor 4'
'd maskout(rmspdb,2.0-abs(rmspdb))'
'draw title 'dtype'_spd,Bias(O-B) at reg. 'he','levz.iz'mb avgd over 'rdate 
'printim 'dtype'_sp_rmeanb_region'he'_lev'levz.iz'.png 'xsize' 'ysize
endif
if(var = rmspda) 
'set ccolor 2'
'd maskout(rmspda,abs(rmspda)-2.0)'
'set ccolor 4'
'd maskout(rmspda,2.0-abs(rmspda))'
'draw title 'dtype'_spd,Bias(O-A) at reg. 'he','levz.iz'mb avgd over 'rdate 
'printim 'dtype'_sp_rmeana_region'he'_lev'levz.iz'.png 'xsize' 'ysize
endif

*'printim 'dtype'_'var'_region'he'_ht'iz'.png 'xsize' 'ysize
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
if(plotfile = uv220); 'set lat 30 75';'set lon 0 80';endif
if(plotfile = uv221); 'set lat 0 90';'set lon 0 180';endif
if(plotfile = uv223); 'set lat 20 50';'set lon 240 300';endif
if(plotfile = uv224); 'set lat 20 67';'set lon 170 241';endif
if(plotfile = uv228); 'set lat 25 65';'set lon 120 200';endif
if(plotfile = uv229); 'set lat -5 30';'set lon 90 160';endif
if(plotfile = uv280 ); 'set lat 50 70'; 'set lon 0 30';endif
if(plotfile = uv281  | plotfile = uv287 );'set lat 42 50'; 'set lon 0 14' ;endif
if(plotfile = uv288  ); 'set lat 18.5 23'; 'set lon 200 207';endif
endif
if(he = 3)
if(plotfile = uv220); 'set lat 0 75';'set lon 120 260';endif
if(plotfile = uv221); 'set lat 0 90';'set lon 180 360';endif
if(plotfile = uv224); 'set lat 23 50';'set lon 240 295';endif
if(plotfile = uv280 ); 'set lat -5 60'; 'set lon 130 240' ;endif
if(plotfile = uv281  | plotfile = uv287 ); 'set lat 50 70'; 'set lon 0 30';endif
if(plotfile = uv288  ); 'set lat 52 68 '; 'set lon 170 220';endif
endif
if(he = 4)
if(plotfile = uv220);'set lat 20 65';'set lon 240 320' ;endif
if(plotfile = uv221); 'set lat -90 0';'set lon 0 180';endif
if(plotfile = uv280 );'set lat 0 30'; 'set lon 240 300' ;endif
if(plotfile = uv281  | plotfile = uv287 ); 'set lat 18 70'; 'set lon 133 240';endif
if(plotfile = uv288  ); 'set lat 34.5 37.5 '; 'set lon 236 240';endif
endif
if(he = 5)
if(plotfile = uv220);'set lat 20 65';'set lon 280 360' ;endif
if(plotfile = uv221); 'set lat -90 0';'set lon 180 360';endif
if(plotfile = uv280 ); 'set lat 30 60'; 'set lon 240 300';endif
if(plotfile = uv281  | plotfile = uv287 ); 'set lat 21 35'; 'set lon 240 282';endif
if(plotfile = uv288  ); 'set lat 37.5 40.5'; 'set lon 236 240';endif
endif
if(he = 6)
if(plotfile = uv220); 'set lat -65 20';'set lon 200 360';endif
if(plotfile = uv280 );'set lat 6 32'; 'set lon 300 335' ;endif
if(plotfile = uv281  | plotfile = uv287 );'set lat 0 21'; 'set lon  254 300' ;endif
if(plotfile = uv288 ); 'set lat 40.5 45.5'; 'set lon 232 240';endif
endif
if(he = 7)
if(plotfile = uv280 ); 'set lat 32 65'; 'set lon 300 360';endif
if(plotfile = uv281  | plotfile = uv287 ); 'set lat 35 50'; 'set lon 240 270';endif
if(plotfile = uv288); 'set lat 45.5 60.0'; 'set lon 220 240';endif
endif
if(he = 8)
if(plotfile = uv281  | plotfile = uv287 ); 'set lat 35 50'; 'set lon 270 300';endif
if(plotfile = uv288 ); 'set lat 45.5 49.5'; 'set lon 235 240';endif
endif
if(he = 9)
if(plotfile = uv281  | plotfile = uv287 ); 'set lat 50 80'; 'set lon 240 300';endif
if(plotfile = uv288 ); 'set lat 0 30'; 'set lon 240 300';endif
endif
if(he = 10)
if(plotfile = uv281 | plotfile = uv287 ); 'set lat 0 30'; 'set lon 300 360';endif
if(plotfile = uv288 ); 'set lat 15 30'; 'set lon 270 300';endif
endif
if(he = 11)
if(plotfile = uv281 | plotfile = uv287 ); 'set lat 30 60'; 'set lon 300 360';endif
if(plotfile = uv288 ); 'set lat 17.5 19.0'; 'set lon 292.5 294.5';endif
endif
if(he = 12)
if(plotfile = uv281 | plotfile = uv287); 'set lat 60 90'; 'set lon 300 360';endif
if(plotfile = uv288 ); 'set lat 24.5 27.0'; 'set lon 275 280';endif
endif
if(he = 13)
if(plotfile = uv281 | plotfile = uv287 ); 'set lat 0 21'; 'set lon  254 300';endif
if(plotfile = uv288 ); 'set lat  27 30'; 'set lon 275 280';endif
endif
if(he = 14)
if(plotfile = uv288 ); 'set lat  27 30'; 'set lon 260 265';endif
endif
if(he = 15)
if(plotfile = uv288 ); 'set lat 32 40'; 'set lon 235 250';endif
endif
if(he = 16)
if(plotfile = uv288 ); 'set lat 32 34.5 '; 'set lon 241 244';endif
endif
if(he = 17)
if(plotfile = uv288 ); 'set lat 32 34 '; 'set lon 247 250';endif
endif
if(he = 18)
if(plotfile = uv288 ); 'set lat 36.5 39 '; 'set lon 237 241';endif
endif
if(he = 19)
if(plotfile = uv288 ); 'set lat 36 38 '; 'set lon 243 247';endif
endif
if(he = 20)
if(plotfile = uv288 ); 'set lat 40 50 '; 'set lon 235 250';endif
endif
if(he = 21)
if(plotfile = uv288 ); 'set lat 40 50 '; 'set lon 235 250';endif
endif
if(he = 22)
if(plotfile = uv288 ); 'set lat 40 50 '; 'set lon 235 250';endif
endif
if(he = 23)
if(plotfile = uv288 ); 'set lat 44 46 '; 'set lon 236 239';endif
endif
if(he = 24)
if(plotfile = uv288 ); 'set lat 47 49 '; 'set lon 236 239';endif
endif
if(he = 25)
if(plotfile = uv288 ); 'set lat 45 47.5 '; 'set lon 239 243';endif
endif
if(he = 26)
if(plotfile = uv288 ); 'set lat 45 47.5 '; 'set lon 239 243';endif
endif
if(he = 27)
if(plotfile = uv288 ); 'set lat 40 41.3 '; 'set lon 246.6 249.2';endif
endif
if(he = 28)
if(plotfile = uv288 ); 'set lat 30 40 '; 'set lon 250 265';endif
endif
if(he = 29)
if(plotfile = uv288 ); 'set lat 31.5 33 '; 'set lon 253 255.5';endif
endif
if(he = 30)
if(plotfile = uv288 ); 'set lat 34.5 35.5 '; 'set lon 253 254.5';endif
endif
if(he = 31)
if(plotfile = uv288 ); 'set lat 38.7 40.0 '; 'set lon 254.4 256.0';endif
endif
if(he = 32)
if(plotfile = uv288 ); 'set lat 30 32 '; 'set lon 261.5 264.8';endif
endif
if(he = 33)
if(plotfile = uv288 ); 'set lat 32.2 33.6 '; 'set lon 262.5 264.8';endif
endif
if(he = 34)
if(plotfile = uv288 ); 'set lat 35.0 38.0 '; 'set lon 261 265';endif
endif
if(he = 35)
if(plotfile = uv288 ); 'set lat 38.0 40.0'; 'set lon 261 265';endif
endif
if(he = 36)
if(plotfile = uv288 ); 'set lat 40.0 50.0'; 'set lon 250 265';endif
endif
if(he = 37)
if(plotfile = uv288 ); 'set lat 40.0 41.2'; 'set lon 253.6 255.7';endif
endif
if(he = 38)
if(plotfile = uv288 ); 'set lat 40.0 45'; 'set lon 257.0 265.0';endif
endif
if(he = 39)
if(plotfile = uv288 ); 'set lat 40.5 42.4'; 'set lon 262.0 265.0';endif
endif
if(he = 40)
if(plotfile = uv288 ); 'set lat 30 40'; 'set lon 265.0 280.0';endif
endif
if(he = 41)
if(plotfile = uv288 ); 'set lat 30 32'; 'set lon 265.0 268.0';endif
endif
if(he = 42)
if(plotfile = uv288 ); 'set lat 30 31.5'; 'set lon 268.5 271.0';endif
endif
if(he = 43)
if(plotfile = uv288 ); 'set lat 30 33'; 'set lon 271.0 276.0';endif
endif
if(he = 44)
if(plotfile = uv288 ); 'set lat 34.2 36.6'; 'set lon 265.5 269.0';endif
endif
if(he = 45)
if(plotfile = uv288 ); 'set lat 38.0 40.0'; 'set lon 265.0 268.0';endif
endif
if(he = 46)
if(plotfile = uv288 ); 'set lat 38.0 40.0'; 'set lon 265.0 268.0';endif
endif
if(he = 47)
if(plotfile = uv288 ); 'set lat 34.5 37.0'; 'set lon 270.0 274.0';endif
endif
if(he = 48)
if(plotfile = uv288 ); 'set lat 37.5 39.9'; 'set lon 268.0 271.5';endif
endif
if(he = 49)
if(plotfile = uv288 ); 'set lat 37.5 40.0'; 'set lon 271.5 274.0';endif
endif
if(he = 50)
if(plotfile = uv288 ); 'set lat 30.0 32.0'; 'set lon 276.0 279.0';endif
endif
if(he = 51)
if(plotfile = uv288 ); 'set lat 33.0 35.0'; 'set lon 272.0 275.0';endif
endif
if(he = 52)
if(plotfile = uv288 ); 'set lat 33.0 35.0'; 'set lon 275.0 278.0';endif
endif
if(he = 53)
if(plotfile = uv288 ); 'set lat 35.0 37.0'; 'set lon 274.0 277.0';endif
endif
if(he = 54)
if(plotfile = uv288 ); 'set lat 35.0 37.0'; 'set lon 277.0 280.0';endif
endif
if(he = 55)
if(plotfile = uv288 ); 'set lat 38.0 40.0'; 'set lon 272.0 275.0';endif
endif
if(he = 56)
if(plotfile = uv288 ); 'set lat 38.0 40.0'; 'set lon 275.0 278.0';endif
endif
if(he = 57)
if(plotfile = uv288 ); 'set lat 40.0 50.0'; 'set lon 265.0 280.0';endif
endif
if(he = 58)
if(plotfile = uv288 ); 'set lat 40.0 44.0'; 'set lon 265.0 270.0';endif
endif
if(he = 59)
if(plotfile = uv288 ); 'set lat 40.0 42.0'; 'set lon 270.0 273.5';endif
endif
if(he = 60)
if(plotfile = uv288 ); 'set lat 40.0 42.0'; 'set lon 273.5 277.0';endif
endif
if(he = 61)
if(plotfile = uv288 ); 'set lat 42.0 44.5'; 'set lon 270.0 274.0';endif
endif
if(he = 62)
if(plotfile = uv288 ); 'set lat 42.0 44.0'; 'set lon 274.0 277.5';endif
endif
if(he = 63)
if(plotfile = uv288 ); 'set lat 44.4 47.0'; 'set lon 265.5 269.5';endif
endif
if(he = 64)
if(plotfile = uv288 ); 'set lat 44.0 46.0'; 'set lon 274.0 277.0';endif
endif
if(he = 65)
if(plotfile = uv288 ); 'set lat 30.0 40.0'; 'set lon 280.0 295.0';endif
endif
if(he = 66)
if(plotfile = uv288 ); 'set lat 32.7 35.5'; 'set lon 280.0 284.5';endif
endif
if(he = 67)
if(plotfile = uv288  ); 'set lat 35.5 38.0'; 'set lon 280.0 284.5';endif
endif
if(he = 68)
if(plotfile = uv288 ); 'set lat 38.0 40.0'; 'set lon 283.0 286.0';endif
endif
if(he = 69)
if(plotfile = uv288  ); 'set lat 38.0 40.0'; 'set lon 283.0 285.0';endif
endif
if(he = 70)
if(plotfile = uv288  ); 'set lat 40.0 50.0'; 'set lon 280.0 300.0';endif
endif
if(he = 71)
if(plotfile = uv288  ); 'set lat 40.0 42.0'; 'set lon 280.0 283.0';endif
endif
if(he = 72)
if(plotfile = uv288 ); 'set lat 42.0 44.0'; 'set lon 280.0 283.0';endif
endif
if(he = 73)
if(plotfile = uv288 ); 'set lat 40.0 42.0'; 'set lon 283.0 286.0';endif
endif
if(he = 74)
if(plotfile = uv288 ); 'set lat 42.0 45.0'; 'set lon 283.0 287.0';endif
endif
if(he = 75)
if(plotfile = uv288 ); 'set lat 40.5 42.5'; 'set lon 286.0 289.0';endif
endif
if(he = 76)
if(plotfile = uv288  ); 'set lat 42.5 45.0'; 'set lon 286.0 290.0';endif
endif

return
