
    datadir=/ptmp/Xiujuan.Su/select_reg
stype="ps120 ps180 ps181 ps187 ps188 q120 q180 q181 q187 q188 t120 t180 t181 t187 t188 uv220 uv221 uv223 uv224 uv229 uv280 uv281 uv287 uv288"

   for dtype in $stype 
    do
  ndata=0
       if [ -s ${datadir}/00/${dtype}_bias_list ]; then
          fileexist00 = .true. 
          ndata=`expr $ndata + 1`
       else 
          fileexist00 = .false.
       fi
       if [ -s ${datadir}/03/${dtype}_bias_list ]; then
          fileexist03 = .true. 
          ndata=`expr $ndata + 1`
       else 
          fileexist03 = .false.
       fi
       if [ -s ${datadir}/06/${dtype}_bias_list ]; then
          fileexist06 = .true. 
          ndata=`expr $ndata + 1`
       else 
          fileexist06 = .false.
       fi
       if [ -s ${datadir}/09/${dtype}_bias_list ]; then
          fileexist09 = .true. 
          ndata=`expr $ndata + 1`
       else 
          fileexist09 = .false.
       fi
       if [ -s ${datadir}/12/${dtype}_bias_list ]; then
          fileexist12 = .true. 
          ndata=`expr $ndata + 1`
       else 
          fileexist12 = .false.
       fi

    cat <<EOF >input
      &input
     ndata=$ndata, fileexist(1)=$fileexist00,fileexist(2)=$fileexist03,
     fileexist(3)=$fileexist06,fileexist(4)=$fileexist09,
     fileexist(5)=$fileexist12,dtype='$dtype',filetype= 'bias'
/
EOF
 
  
