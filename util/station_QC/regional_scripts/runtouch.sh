#!/bin/sh
set -xa

export tmpdir1=/ptmpp1/$USER/gsiqc3/bufrstas_regional
export tmpdir2=/ptmpp1/$USER/gsiqc3/select_reg


for tm in 00 03 06 
do
touch ${tmpdir1}/${tm}/*
touch ${tmpdir2}/${tm}/*
done

exit
