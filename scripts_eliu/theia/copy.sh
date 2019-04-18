#!/bin/ksh 


target=$1

dir_a=/scratch4/NCEPDEV/da/save/Emily.Liu/GSI/ProdGSI-fv3mp/src

dir_b=/scratch4/NCEPDEV/da/save/Emily.Liu/GSI/old/ProdGSI-fv3mp/src


diff -b -w -s  ${dir_a}/${target} ${dir_b}/${target}

cp ${dir_b}/${target} ${dir_a}/${target}

diff -b -w -s  ${dir_a}/${target} ${dir_b}/${target}


