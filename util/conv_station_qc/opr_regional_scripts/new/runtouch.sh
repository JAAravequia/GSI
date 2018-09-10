#!/bin/sh
set -xa

export tmpdir1=/ptmp/Xiujuan.Su/bufrstas_regional
export tmpdir2=/ptmp/Xiujuan.Su/select_reg


for tm in 00 03 06 09 12
do
touch ${tmpdir1}/${tm}/*
touch ${tmpdir2}/${tm}/*
done

exit
