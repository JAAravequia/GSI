
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
if (dtype= ps180 | dtype= q180 | dtype= t180); nhe=7;endif
if (dtype = ps183 ); nhe=9;endif
if (dtype = ps120 ); nhe=6;endif
if (dtype = ps181 | dtype = q181  | dtype = t181); nhe=12;endif
if (dtype = ps187 | dtype = q187  | dtype = t187); nhe=12;endif
if (dtype = ps188 | dtype = q188  | dtype = t188); nhe=79;endif


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

he=she
**'run setvpage 1 2 2 2 0.9'
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
'draw title 'dtype',data No. rejected at region 'he
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
if(plotfile = ps181 | plotfile = q181 | plotfile =t181); 'set lat 42 50'; 'set lon 0 14' ;endif
if(plotfile = ps187 | plotfile = q187 | plotfile =t187); 'set lat 42 50'; 'set lon 0 14' ;endif
if(plotfile = ps183); 'set lat 0 35'; 'set lon 0 70';endif
if(plotfile = ps188 | plotfile = q188 | plotfile =t188); 'set lat 40 50'; 'set lon 0 20';endif
endif
if(he = 3)
if(plotfile = ps120); 'set lat 0 75';'set lon 120 260';endif
if(plotfile = ps180 | plotfile = t180 | plotfile = q180); 'set lat -5 60'; 'set lon 130 240';endif
if(plotfile = ps181 | plotfile = q181 | plotfile = t181); 'set lat 50 70'; 'set lon 0 30' ;endif
if(plotfile = ps187 | plotfile = q187 | plotfile = t187); 'set lat 50 70'; 'set lon 0 30' ;endif
if(plotfile = ps183); 'set lat 35 70'; 'set lon 0 70';endif
if(plotfile = ps188 | plotfile = q188 | plotfile = t188); 'set lat 50 57'; 'set lon 0 14';endif
endif
if(he = 4)
if(plotfile = ps120); 'set lat 20 65';'set lon 240 320';endif
if(plotfile = ps180 | plotfile = t180 | plotfile = q180); 'set lat 0 30'; 'set lon 240 300';endif
if(plotfile = ps181 | plotfile = q181 | plotfile = t181); 'set lat 18 70'; 'set lon 133 240' ;endif
if(plotfile = ps187 | plotfile = q187 | plotfile = t187); 'set lat 18 70'; 'set lon 133 240' ;endif
if(plotfile = ps183); 'set lat 0 35'; 'set lon 70 130';endif
if(plotfile = ps188 | plotfile = q188 | plotfile = t188); 'set lat 57 70'; 'set lon 0 20';endif
endif
if(he = 5)
if(plotfile = ps120); 'set lat 20 65';'set lon 280 360';endif
if(plotfile = ps180 | plotfile = t180 | plotfile = q180); 'set lat 30 60'; 'set lon 240 300';endif
if(plotfile = ps181 | plotfile = q181 | plotfile = t181);'set lat 21 35'; 'set lon 240 282' ;endif
if(plotfile = ps187 | plotfile = q187 | plotfile = t187);'set lat 21 35'; 'set lon 240 282' ;endif
if(plotfile = ps183);'set lat 35 70'; 'set lon 75 135';endif
if(plotfile = ps188 | plotfile = q188 | plotfile = t188); 'set lat 18.5 23'; 'set lon 200 207';endif
endif
if(he = 6)
if(plotfile = ps120); 'set lat -65 20';'set lon 200 360';endif
if(plotfile = ps180 | plotfile = t180 | plotfile = q180); 'set lat 0 32'; 'set lon 300 360';endif
if(plotfile = ps181 | plotfile = q181 | plotfile = t181); 'set lat 0 21'; 'set lon  254 300';endif
if(plotfile = ps187 | plotfile = q187 | plotfile = t187); 'set lat 0 21'; 'set lon  254 300';endif
if(plotfile = ps183);'set lat 35 75'; 'set lon 220 300';endif
*if(plotfile = ps183);'set lat 0 40'; 'set lon 200 280';endif
if(plotfile = ps188 | plotfile = q188 | plotfile = t188); 'set lat 52 68 '; 'set lon 170 220';endif
endif
if(he = 7)
if(plotfile = ps180 | plotfile = t180 | plotfile = q180); 'set lat 32 65'; 'set lon 300 360';endif
if(plotfile = ps181 | plotfile = q181 | plotfile = t181); 'set lat 35 50'; 'set lon 240 270';endif
if(plotfile = ps187 | plotfile = q187 | plotfile = t187); 'set lat 35 50'; 'set lon 240 270';endif
if(plotfile = ps181 | plotfile = q181 | plotfile = t181); 'set lat 50 80'; 'set lon 240 300';endif
if(plotfile = ps187 | plotfile = q187 | plotfile = t187); 'set lat 50 80'; 'set lon 240 300';endif
if(plotfile = ps183);'set lat 30 65'; 'set lon 290 360';endif
if(plotfile = ps188 | plotfile = q188 | plotfile = t188); 'set lat 34.5 37.5 '; 'set lon 236 240';endif
endif
if(he = 8)
if(plotfile = ps181 | plotfile = q181 | plotfile = t181); 'set lat 35 50'; 'set lon 270 300';endif
if(plotfile = ps187 | plotfile = q187 | plotfile = t187); 'set lat 35 50'; 'set lon 270 300';endif
if(plotfile = ps183);'set lat -90 0'; 'set lon 0 180';endif
if(plotfile = ps188 | plotfile = q188 | plotfile = t188); 'set lat 37.5 40.5'; 'set lon 236 240';endif
endif
if(he = 9)
if(plotfile = ps181 | plotfile = q181 | plotfile = t181); 'set lat 50 80'; 'set lon 240 300';endif
if(plotfile = ps187 | plotfile = q187 | plotfile = t187); 'set lat 50 80'; 'set lon 240 300';endif
if(plotfile = ps183);'set lat -90 0'; 'set lon 180 360';endif
if(plotfile = ps188 | plotfile = q188 | plotfile = t188); 'set lat 40.5 45.5'; 'set lon 232 240';endif
endif
if(he = 10)
if(plotfile = ps181 | plotfile = q181 | plotfile = t181); 'set lat 0 30'; 'set lon 300 360';endif
if(plotfile = ps187 | plotfile = q187 | plotfile = t187); 'set lat 0 30'; 'set lon 300 360';endif
if(plotfile = ps188 | plotfile = q188 | plotfile = t188); 'set lat 45.5 60.0'; 'set lon 220 240';endif
endif
if(he = 11)
if(plotfile = ps181 | plotfile = q181 | plotfile = t181 ); 'set lat 30 60'; 'set lon 300 360';endif
if(plotfile = ps187 | plotfile = q187 | plotfile = t187 ); 'set lat 30 60'; 'set lon 300 360';endif
if(plotfile = ps188 | plotfile = q188 | plotfile = t188); 'set lat 45.5 49.5'; 'set lon 235 240';endif
endif
if(he = 12)
if(plotfile = ps181 | plotfile = q181 | plotfile = t181); 'set lat 60 90'; 'set lon 300 360';endif
if(plotfile = ps187 | plotfile = q187 | plotfile = t187); 'set lat 60 90'; 'set lon 300 360';endif
if(plotfile = ps188 | plotfile = q188 | plotfile = t188); 'set lat 0 30'; 'set lon 240 300';endif
endif
if(he = 13)
if(plotfile = ps187 | plotfile = q187 | plotfile = t187); 'set lat 0 21'; 'set lon  254 300';endif
if(plotfile = ps181 | plotfile = q181 | plotfile = t181); 'set lat 0 21'; 'set lon  254 300';endif
if(plotfile = ps188 | plotfile = q188 | plotfile = t188); 'set lat 15 30'; 'set lon 270 300';endif
endif
if(he = 14)
if(plotfile = ps188 | plotfile = q188 | plotfile = t188); 'set lat 17.5 19.0'; 'set lon 292.5 294.5';endif
endif
if(he = 15)
if(plotfile = ps188 | plotfile = q188 | plotfile = t188); 'set lat 24.5 27.0'; 'set lon 275 280';endif
endif
if(he = 16)
if(plotfile = ps188 | plotfile = q188 | plotfile = t188); 'set lat  27 30'; 'set lon 275 280';endif
endif
if(he = 17)
if(plotfile = ps188 | plotfile = q188 | plotfile = t188); 'set lat  27 30'; 'set lon 260 265';endif
endif
if(he = 18)
if(plotfile = ps188 | plotfile = q188 | plotfile = t188); 'set lat 32 40'; 'set lon 235 250';endif
endif
if(he = 19)
if(plotfile = ps188 | plotfile = q188 | plotfile = t188); 'set lat 32 34.5 '; 'set lon 241 244';endif
endif
if(he = 20)
if(plotfile = ps188 | plotfile = q188 | plotfile = t188); 'set lat 32 34 '; 'set lon 247 250';endif
endif
if(he = 21)
if(plotfile = ps188 | plotfile = q188 | plotfile = t188); 'set lat 36.5 39 '; 'set lon 237 241';endif
endif
if(he = 22)
if(plotfile = ps188 | plotfile = q188 | plotfile = t188 ); 'set lat 36 38 '; 'set lon 243 247';endif
endif
if(he = 23)
if(plotfile = ps188 | plotfile = q188 | plotfile = t188 ); 'set lat 40 50 '; 'set lon 235 250';endif
endif
if(he = 24)
if(plotfile = ps188 | plotfile = q188 | plotfile = t188 ); 'set lat 40 50 '; 'set lon 235 250';endif
endif
if(he = 25)
if(plotfile = ps188 | plotfile = q188 | plotfile = t188 ); 'set lat 40 50 '; 'set lon 235 250';endif
endif
if(he = 26)
if(plotfile = ps188 | plotfile = q188 | plotfile = t188 ); 'set lat 44 46 '; 'set lon 236 239';endif
endif
if(he = 27)
if(plotfile = ps188 | plotfile = q188 | plotfile = t188 ); 'set lat 47 49 '; 'set lon 236 239';endif
endif
if(he = 28)
if(plotfile = ps188 | plotfile = q188 | plotfile = t188 ); 'set lat 45 47.5 '; 'set lon 239 243';endif
endif
if(he = 29)
if(plotfile = ps188 | plotfile = q188 | plotfile = t188 ); 'set lat 45 47.5 '; 'set lon 239 243';endif
endif
if(he = 30)
if(plotfile = ps188 | plotfile = q188 | plotfile = t188 ); 'set lat 40 41.3 '; 'set lon 246.6 249.2';endif
endif
if(he = 31)
if(plotfile = ps188 | plotfile = q188 | plotfile = t188 ); 'set lat 30 40 '; 'set lon 250 265';endif
endif
if(he = 32)
if(plotfile = ps188 | plotfile = q188 | plotfile = t188 ); 'set lat 31.5 33 '; 'set lon 253 255.5';endif
endif
if(he = 33)
if(plotfile = ps188 | plotfile = q188 | plotfile = t188 ); 'set lat 34.5 35.5 '; 'set lon 253 254.5';endif
endif
if(he = 34)
if(plotfile = ps188 | plotfile = q188 | plotfile = t188 ); 'set lat 38.7 40.0 '; 'set lon 254.4 256.0';endif
endif
if(he = 35)
if(plotfile = ps188 | plotfile = q188 | plotfile = t188 ); 'set lat 30 32 '; 'set lon 261.5 264.8';endif
endif
if(he = 36)
if(plotfile = ps188 | plotfile = q188 | plotfile = t188 ); 'set lat 32.2 33.6 '; 'set lon 262.5 264.8';endif
endif
if(he = 37)
if(plotfile = ps188 | plotfile = q188 | plotfile = t188 ); 'set lat 35.0 38.0 '; 'set lon 261 265';endif
endif
if(he = 38)
if(plotfile = ps188 | plotfile = q188 | plotfile = t188 ); 'set lat 38.0 40.0'; 'set lon 261 265';endif
endif
if(he = 39)
if(plotfile = ps188 | plotfile = q188 | plotfile = t188 ); 'set lat 40.0 50.0'; 'set lon 250 265';endif
endif
if(he = 40)
if(plotfile = ps188 | plotfile = q188 | plotfile = t188 ); 'set lat 40.0 41.2'; 'set lon 253.6 255.7';endif
endif
if(he = 41)
if(plotfile = ps188 | plotfile = q188 | plotfile = t188 ); 'set lat 40.0 45'; 'set lon 257.0 265.0';endif
endif
if(he = 42)
if(plotfile = ps188 | plotfile = q188 | plotfile = t188 ); 'set lat 40.5 42.4'; 'set lon 262.0 265.0';endif
endif
if(he = 43)
if(plotfile = ps188 | plotfile = q188 | plotfile = t188 ); 'set lat 30 40'; 'set lon 265.0 280.0';endif
endif
if(he = 44)
if(plotfile = ps188 | plotfile = q188 | plotfile = t188 ); 'set lat 30 32'; 'set lon 265.0 268.0';endif
endif
if(he = 45)
if(plotfile = ps188 | plotfile = q188 | plotfile = t188 ); 'set lat 30 31.5'; 'set lon 268.5 271.0';endif
endif
if(he = 46)
if(plotfile = ps188 | plotfile = q188 | plotfile = t188 ); 'set lat 30 33'; 'set lon 271.0 276.0';endif
endif
if(he = 47)
if(plotfile = ps188 | plotfile = q188 | plotfile = t188 ); 'set lat 34.2 36.6'; 'set lon 265.5 269.0';endif
endif
if(he = 48)
if(plotfile = ps188 | plotfile = q188 | plotfile = t188 ); 'set lat 38.0 40.0'; 'set lon 265.0 268.0';endif
endif
if(he = 49)
if(plotfile = ps188 | plotfile = q188 | plotfile = t188 ); 'set lat 38.0 40.0'; 'set lon 265.0 268.0';endif
endif
if(he = 50)
if(plotfile = ps188 | plotfile = q188 | plotfile = t188 ); 'set lat 34.5 37.0'; 'set lon 270.0 274.0';endif
endif
if(he = 51)
if(plotfile = ps188 | plotfile = q188 | plotfile = t188 ); 'set lat 37.5 39.9'; 'set lon 268.0 271.5';endif
endif
if(he = 52)
if(plotfile = ps188 | plotfile = q188 | plotfile = t188 ); 'set lat 37.5 40.0'; 'set lon 271.5 274.0';endif
endif
if(he = 53)
if(plotfile = ps188 | plotfile = q188 | plotfile = t188 ); 'set lat 30.0 32.0'; 'set lon 276.0 279.0';endif
endif
if(he = 54)
if(plotfile = ps188 | plotfile = q188 | plotfile = t188 ); 'set lat 33.0 35.0'; 'set lon 272.0 275.0';endif
endif
if(he = 55)
if(plotfile = ps188 | plotfile = q188 | plotfile = t188 ); 'set lat 33.0 35.0'; 'set lon 275.0 278.0';endif
endif
if(he = 56)
if(plotfile = ps188 | plotfile = q188 | plotfile = t188 ); 'set lat 35.0 37.0'; 'set lon 274.0 277.0';endif
endif
if(he = 57)
if(plotfile = ps188 | plotfile = q188 | plotfile = t188 ); 'set lat 35.0 37.0'; 'set lon 277.0 280.0';endif
endif
if(he = 58)
if(plotfile = ps188 | plotfile = q188 | plotfile = t188 ); 'set lat 38.0 40.0'; 'set lon 272.0 275.0';endif
endif
if(he = 59)
if(plotfile = ps188 | plotfile = q188 | plotfile = t188 ); 'set lat 38.0 40.0'; 'set lon 275.0 278.0';endif
endif
if(he = 60)
if(plotfile = ps188 | plotfile = q188 | plotfile = t188 ); 'set lat 40.0 50.0'; 'set lon 265.0 280.0';endif
endif
if(he = 61)
if(plotfile = ps188 | plotfile = q188 | plotfile = t188 ); 'set lat 40.0 44.0'; 'set lon 265.0 270.0';endif
endif
if(he = 62)
if(plotfile = ps188 | plotfile = q188 | plotfile = t188 ); 'set lat 40.0 42.0'; 'set lon 270.0 273.5';endif
endif
if(he = 63)
if(plotfile = ps188 | plotfile = q188 | plotfile = t188 ); 'set lat 40.0 42.0'; 'set lon 273.5 277.0';endif
endif 
if(he = 64)
if(plotfile = ps188 | plotfile = q188 | plotfile = t188 ); 'set lat 42.0 44.5'; 'set lon 270.0 274.0';endif
endif 
if(he = 65)
if(plotfile = ps188 | plotfile = q188 | plotfile = t188 ); 'set lat 42.0 44.0'; 'set lon 274.0 277.5';endif
endif 
if(he = 66)
if(plotfile = ps188 | plotfile = q188 | plotfile = t188 ); 'set lat 44.4 47.0'; 'set lon 265.5 269.5';endif
endif 
if(he = 67)
if(plotfile = ps188 | plotfile = q188 | plotfile = t188 ); 'set lat 44.0 46.0'; 'set lon 274.0 277.0';endif
endif 
if(he = 68)
if(plotfile = ps188 | plotfile = q188 | plotfile = t188 ); 'set lat 30.0 40.0'; 'set lon 280.0 295.0';endif
endif 
if(he = 69)
if(plotfile = ps188 | plotfile = q188 | plotfile = t188 ); 'set lat 32.7 35.5'; 'set lon 280.0 284.5';endif
endif 
if(he = 70)
if(plotfile = ps188 | plotfile = q188 | plotfile = t188 ); 'set lat 35.5 38.0'; 'set lon 280.0 284.5';endif
endif 
if(he = 71)
if(plotfile = ps188 | plotfile = q188 | plotfile = t188 ); 'set lat 38.0 40.0'; 'set lon 283.0 286.0';endif
endif 
if(he = 72)
if(plotfile = ps188 | plotfile = q188 | plotfile = t188 ); 'set lat 38.0 40.0'; 'set lon 283.0 285.0';endif
endif 
if(he = 73)
if(plotfile = ps188 | plotfile = q188 | plotfile = t188 ); 'set lat 40.0 50.0'; 'set lon 280.0 300.0';endif
endif 
if(he = 74)
if(plotfile = ps188 | plotfile = q188 | plotfile = t188 ); 'set lat 40.0 42.0'; 'set lon 280.0 283.0';endif
endif 
if(he = 75)
if(plotfile = ps188 | plotfile = q188 | plotfile = t188 ); 'set lat 42.0 44.0'; 'set lon 280.0 283.0';endif
endif 
if(he = 76)
if(plotfile = ps188 | plotfile = q188 | plotfile = t188 ); 'set lat 40.0 42.0'; 'set lon 283.0 286.0';endif
endif 
if(he = 77)
if(plotfile = ps188 | plotfile = q188 | plotfile = t188 ); 'set lat 42.0 45.0'; 'set lon 283.0 287.0';endif
endif 
if(he = 78)
if(plotfile = ps188 | plotfile = q188 | plotfile = t188 ); 'set lat 40.5 42.5'; 'set lon 286.0 289.0';endif
endif 
if(he = 79)
if(plotfile = ps188 | plotfile = q188 | plotfile = t188 ); 'set lat 42.5 45.0'; 'set lon 286.0 290.0';endif
endif 

return

