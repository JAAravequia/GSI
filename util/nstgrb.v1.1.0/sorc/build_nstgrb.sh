#! /usr/bin/env bash
set -eux

module purge
source ../modulefiles/nstgrb.wcoss_dell_p3  
module list

cd ./nstgrb.fd
./makefile.sh
