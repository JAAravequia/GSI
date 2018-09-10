#!/bin/sh
set -xa

### define the script, data directory and excutable
### file for each run

export dtime=$1
export scripts=$2
export datadir=$3
export datadir2=$4
export exec=$5
export execfile=$6
export execfile1=$7
export savedir=$8
export sfctype=${9}
 export sondtype=${10}
 export uvsfctype=${11}
 export uvsondtype=${12}
 export alltm=${13}
# dtime=20120103
# export scripts=/u/Xiujuan.Su/home/gsiqc3/regional_scripts
# export datadir=/u/Xiujuan.Su/nbns/select_reg_20120103
# export datadir2=/u/Xiujuan.Su/nbns/bufrstas_regional_20120103 
# export exec=/u/Xiujuan.Su/home/gsiqc3/exec
# export execfile=compare_list.x
# export execfile1=compare_list_sp.x
# export savedir=/u/Xiujuan.Su/home/gsiqc3/regional_data/$dtime

export tmpdir=/ptmpp1/$USER/gsiqc3/compare_list

mkdir -p $savedir
mkdir -p $tmpdir

cd $tmpdir

rm -f $tmpdir/*

cp $exec/$execfile ./$execfile
cp $exec/$execfile1 ./$execfile1
   

     notm=0
     for ttm in ${alltm}; do
        notm=`expr $notm + 1`
     done

     if [ $notm = 1 ]; then
        for datatype in ssfctype ssondtype ; do
           if [ "${datatype}" = "ssfctype" ]; then
              dstype=$sfctype
              itype=0
           elif [ "${datatype}" = "ssondtype" ]; then
              dstype=$sondtype
              itype=1
           fi
           for ttm in ${alltm}; do
              for dtype in $dstype; do
                 for filetype in bias rej; do
                    cp ${datadir}/${ttm}/${dtype}_${filetype}_list $savedir/${dtype}_rej_${filetype}_list 
                 done
              done
           done
        done
     elif [ $notm = 2 ]; then
        for datatype in ssfctype ssondtype; do
           if [ "${datatype}" = "ssfctype" ]; then
              dstype=$sfctype
              itype=0
           elif [ "${datatype}" = "ssondtype" ]; then
              dstype=$sondtype
              itype=1
           fi
           for dtype in $dstype; do 
              for tm in ${alltm}; do 
                 cp ${datadir2}/${tm}/${dtype}_station ./${dtype}_station_${tm} 
                 cp ${datadir2}/${tm}/${dtype}_stas ./${dtype}_stas_${tm} 
              done 
              for filetype in bias rej;  do
                 mtm=0
                 nfile=0
                 for tm in ${alltm}; do
                    mtm=`expr ${mtm} + 1`
                    echo ${mtm} 
                    if [ ${mtm}  = 1 ]; then 
                       tm1=${tm}
                       if [ -s ${datadir}/${tm}/${dtype}_${filetype}_list ]; then
                          export fexist1=.true.
                          cp ${datadir}/${tm}/${dtype}_${filetype}_list ./${dtype}_${filetype}_list_${tm}
                          nfile=`expr ${nfile} + 1`
                          sign1=1
                       else
                          export fexist1=.false.
                          sign1=0
                       fi
                    elif [ ${mtm}  = 2 ]; then
                       tm2=${tm}
                       if [ -s ${datadir}/${tm}/${dtype}_${filetype}_list ]; then
                          export fexist2=.true.
                          cp ${datadir}/${tm}/${dtype}_${filetype}_list ./${dtype}_${filetype}_list_${tm}
                          nfile=`expr ${nfile} + 1`
                          sign2=1
                       else
                          export fexist2=.false.
                          sign2=0
                       fi
                    fi
                 done
                 rm -f input
                 if [ $nfile = 0  ];then
                    echo 'file no exst ' ${dtype}_${filetype}_list
                 else  
 cat << EOF > input
   &input
   ntm=2,fileexist(1)=${fexist1},fileexist(2)=${fexist2},
   tm(1)='${tm1}',tm(2)='${tm2}',dtype='${dtype}',
   filetype='${filetype}',itype=${itype}
  /
EOF
                 ./$execfile <input >stdout 2>&1
              mv stdout ${dtype}_${filetype}_stdout
              mv ${dtype}_${filetype}_list $savedir
           fi
        done
      done
   done
     elif [ ${notm} = 3 ]; then
        for datatype in ssfctype ssondtype; do
           if [ "${datatype}" = "ssfctype" ]; then
              dstype=$sfctype
              itype=0
           elif [ "${datatype}" = "ssondtype" ]; then
              dstype=$sondtype
              itype=1
           fi
           for dtype in $dstype; do 
              for tm in ${alltm}; do
                 cp ${datadir2}/${tm}/${dtype}_station ./${dtype}_station_${tm}
                 cp ${datadir2}/${tm}/${dtype}_stas ./${dtype}_stas_${tm}
              done
              for filetype in bias rej;  do
                 mtm=0
                 nfile=0
                 for tm in ${alltm}; do
                    mtm=`expr ${mtm} + 1`
                    echo $mtm
                    if [ ${mtm}  = 1 ]; then
                       tm1=${tm}
                       if [ -s ${datadir}/${tm}/${dtype}_${filetype}_list ]; then
                          export fexist1=.true.
                          cp ${datadir}/${tm}/${dtype}_${filetype}_list ./${dtype}_${filetype}_list_${tm}
                          nfile=`expr ${nfile} + 1`
                          sign1=1
                       else
                          export fexist1=.false.
                          sign1=0
                       fi
                    elif [ ${mtm}  = 2 ]; then
                       tm2=${tm}
                       if [ -s ${datadir}/${tm}/${dtype}_${filetype}_list ]; then
                          export fexist2=.true.
                          cp ${datadir}/${tm}/${dtype}_${filetype}_list ./${dtype}_${filetype}_list_${tm}
                          nfile=`expr ${nfile} + 1`
                          sign2=1
                       else
                          export fexist2=.false.
                          sign2=0
                       fi
                    elif [ ${mtm}  = 3 ]; then
                       tm3=${tm}
                       if [ -s ${datadir}/${tm}/${dtype}_${filetype}_list ]; then
                          export fexist3=.true.
                          cp ${datadir}/${tm}/${dtype}_${filetype}_list ./${dtype}_${filetype}_list_${tm}
                          nfile=`expr ${nfile} + 1`
                          sign3=1
                       else
                          export fexist3=.false.
                          sign3=0
                       fi
                    fi
                 done
                 if [ $nfile = 0 ];then
                    echo 'file no exst ' ${dtype}_${filetype}_list 
                 else
                    rm -f input
 cat << EOF > input
  &input
   ntm=3,fileexist(1)=${fexist1},fileexist(2)=${fexist2},fileexist(3)=${fexist3},
   tm(1)='${tm1}',tm(2)='${tm2}',tm(3)='${tm3}',dtype='${dtype}',
   filetype='${filetype}',itype=${itype}
 /
EOF
                    ./$execfile <input >stdout 2>&1
                    mv stdout ${dtype}_${filetype}_stdout
                    mv ${dtype}_${filetype}_list $savedir
                 fi
              done
           done
        done
     fi 

## for wind data type

        if [ $notm = 1 ]; then
           for datatype in suvsfctype suvsondtype; do
           if [ "${datatype}" = "suvsfctype" ]; then
              dstype=$uvsfctype
              itype=0
           elif [ "${datatype}" = "suvsondtype" ]; then
              dstype=$uvsondtype
              itype=1
           fi
           for ttm in ${alltm}; do
              for dtype in $dstype; do
                 for ftype in sp dir; do
                    cp ${datadir}/${ttm}/${dtype}_rej_${ftype}_list $savedir/${dtype}_rej_${ftype}_list 
                 done
              done
           done
        done
     elif [ $notm = 2 ]; then
        for datatype in suvsfctype suvsondtype; do
           if [ "${datatype}" = "suvsfctype" ]; then
              dstype=$uvsfctype
              itype=0
           elif [ "${datatype}" = "suvsondtype" ]; then
              dstype=$uvsondtype
              itype=1
           fi
           for dtype in $dstype; do 
              for tm in ${alltm}; do 
                 cp ${datadir2}/${tm}/${dtype}_station ./${dtype}_station_${tm} 
                 cp ${datadir2}/${tm}/${dtype}_stas ./${dtype}_stas_${tm} 
              done 
              for ftype in sp dir;  do
                 mtm=0
                 nfile=0
                 for tm in ${alltm}; do
                    mtm=`expr ${mtm} + 1`
                    echo ${mtm} 
                    if [ ${mtm}  = 1 ]; then 
                       tm1=${tm}
                       if [ -s ${datadir}/${tm}/${dtype}_rej_${ftype}_list ]; then
                          export fexist1=.true.
                          cp ${datadir}/${tm}/${dtype}_rej_${ftype}_list ./${dtype}_rej_${ftype}_list_${tm}
                          nfile=`expr ${nfile} + 1`
                          sign1=1
                       else
                          export fexist1=.false.
                          sign1=0
                       fi
                    elif [ ${mtm}  = 2 ]; then
                       tm2=${tm}
                       if [ -s ${datadir}/${tm}/${dtype}_rej_${ftype}_list ]; then
                          export fexist2=.true.
                          cp ${datadir}/${tm}/${dtype}_rej_${ftype}_list ./${dtype}_rej_${ftype}_list_${tm}
                          nfile=`expr ${nfile} + 1`
                          sign2=1
                       else
                          export fexist2=.false.
                          sign2=0
                       fi
                    fi
                 done
                 if [ $nfile = 0 ];then
                    echo 'file no exst ' ${dtype}_rej_${ftype}_list
                 else
                rm -f input
 cat << EOF > input
   &input
   ntm=2,fileexist(1)=${fexist1},fileexist(2)=${fexist2},
   tm(1)='${tm1}',tm(2)='${tm2}',dtype='${dtype}',
   filetype='rej',itype=${itype}
  /
EOF
                    if [ "${ftype}" = 'sp' ]; then
                       ./$execfile1<input >stdout 2>&1
                    else
                       ./$execfile <input >stdout 2>&1
                    fi

                    mv stdout ${dtype}_${ftype}_stdout
                    mv ${dtype}_rej_${ftype}_list $savedir
                 fi
              done
           done
        done
     elif [ ${notm} = 3 ]; then
        for datatype in suvsfctype suvsondtype; do
           if [ "${datatype}" = "suvsfctype" ]; then
              dstype=$uvsfctype
              itype=0
           elif [ "${datatype}" = "suvsondtype" ]; then
              dstype=$uvsondtype
              itype=1
           fi
           for dtype in $dstype; do 
              for tm in ${alltm}; do
                 cp ${datadir2}/${tm}/${dtype}_station ./${dtype}_station_${tm}
                 cp ${datadir2}/${tm}/${dtype}_stas ./${dtype}_stas_${tm}
              done
              for ftype in sp dir;  do
                 mtm=0
                 nfile=0
                 for tm in ${alltm}; do
                    mtm=`expr ${mtm} + 1`
                    echo $mtm
                    if [ ${mtm}  = 1 ]; then
                       tm1=${tm}
                       if [ -s ${datadir}/${tm}/${dtype}_rej_${ftype}_list ]; then
                          export fexist1=.true.
                          cp ${datadir}/${tm}/${dtype}_rej_${ftype}_list ./${dtype}_rej_${ftype}_list_${tm}
                          nfile=`expr ${nfile} + 1`
                          sign1=1
                       else
                          export fexist1=.false.
                          sign1=0
                       fi
                    elif [ ${mtm}  = 2 ]; then
                       tm2=${tm}
                       if [ -s ${datadir}/${tm}/${dtype}_rej_${ftype}_list ]; then
                          export fexist2=.true.
                          cp ${datadir}/${tm}/${dtype}_rej_${ftype}_list ./${dtype}_rej_${ftype}_list_${tm}
                          nfile=`expr ${nfile} + 1`
                          sign2=1
                       else
                          export fexist2=.false.
                          sign2=0
                       fi
                    elif [ ${mtm}  = 3 ]; then
                       tm3=${tm}
                       if [ -s ${datadir}/${tm}/${dtype}_rej_${ftype}_list ]; then
                          export fexist3=.true.
                          cp ${datadir}/${tm}/${dtype}_rej_${ftype}_list ./${dtype}_rej_${ftype}_list_${tm}
                          nfile=`expr ${nfile} + 1`
                          sign3=1
                       else
                          export fexist3=.false.
                          sign3=0
                       fi
                    fi
                 done
                 if [ $nfile = 0 ];then
                    echo 'file no exst ' ${dtype}_rej_${ftype}_list
                 else
                 rm -f input
 cat << EOF > input
  &input
   ntm=3,fileexist(1)=${fexist1},fileexist(2)=${fexist2},fileexist(3)=${fexist3},
   tm(1)='${tm1}',tm(2)='${tm2}',tm(3)='${tm3}',dtype='${dtype}',
   filetype='rej',itype=${itype}
 /
EOF
                    if [ "${ftype}" = 'sp' ]; then
                       ./$execfile1<input >stdout 2>&1
                    else
                       ./$execfile <input >stdout 2>&1
                    fi

                    mv stdout ${dtype}_rej_${ftype}_stdout
                    mv ${dtype}_rej_${ftype}_list $savedir
                 fi
    
              done
           done
        done
     fi 



exit
