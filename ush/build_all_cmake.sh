#!/bin/sh

set -ex

cd ..
pwd=$(pwd)

target=$1
dir_root=${2:-$pwd}

if [ $target = wcoss ]; then
    . /usrx/local/Modules/3.2.10/init/sh
    conf_target=nco
elif [ $target = cray -o $target = wcoss_c ]; then
    . $MODULESHOME/init/sh
    conf_target=nco
elif [ $target = theia ]; then
    . /apps/lmod/lmod/init/sh
    conf_target=theia
elif [ $target = gaea                      ]; then
    . $MODULESHOME/init/sh
    conf_target=gaea
else
    echo "unknown target = $target"
    exit 9
fi

dir_modules=$dir_root/modulefiles
if [ ! -d $dir_modules ]; then
    echo "modulefiles does not exist in $dir_modules"
    exit 10
fi
[ -d $dir_root/exec ] || mkdir -p $dir_root/exec

rm -rf $dir_root/build
mkdir -p $dir_root/build
cd $dir_root/build

module purge
if [ $target = gaea ]; then
    unset _LMFILES_
    unset _LMFILES_000
    unset _LMFILES_001
    unset LOADEDMODULES

    #module use -a /opt/cray/ari/modulefiles
    #module use -a /opt/cray/pe/ari/modulefiles
    #module use -a /opt/cray/pe/craype/default/modulefiles
    #source /etc/opt/cray/pe/admin-pe/site-config

    module use $dir_modules
    module load modulefile.global_gsi.$target
    module load cmake
    FV3GFS_BUILD='-DBUILD_UTIL=OFF -DBUILD_CORELIBS=OFF'
else
    FV3GFS_BUILD='-DBUILD_UTIL=ON'
fi

if [ $target = wcoss -o $target = cray -o $target = gaea ]; then
    module load $dir_modules/modulefile.ProdGSI.$target
else
    source $dir_modules/modulefile.ProdGSI.$target
fi
module list

cmake $DONOT_BUILD_CORE $FV3GFS_BUILD -DCMAKE_BUILD_TYPE=PRODUCTION ..

make -j 8

exit
