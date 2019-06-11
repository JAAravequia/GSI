#!/bin/sh

set -ex

cd ..
pwd=$(pwd)

build_type=${1:-'PRODUCTION'}
dir_root=${2:-$pwd}

if [[ -d /dcom && -d /hwrf ]] ; then
    . /usrx/local/Modules/3.2.10/init/sh
    target=wcoss
    . $MODULESHOME/init/sh
elif [[ -d /cm ]] ; then
    . $MODULESHOME/init/sh
    target=wcoss_c
elif [[ -d /ioddev_dell ]]; then
    . $MODULESHOME/init/sh
    target=wcoss_d
elif [[ -d /scratch3 ]] ; then
    . /apps/lmod/lmod/init/sh
    target=theia
elif [[ -d /carddata ]] ; then
    . /opt/apps/lmod/3.1.9/init/sh
    target=s4
elif [[ -d /jetmon ]] ; then
    . $MODULESHOME/init/sh
    target=jet
elif [[ -d /glade ]] ; then
    . $MODULESHOME/init/sh
    target=cheyenne
elif [[ -d /sw/gaea ]] ; then
    . /opt/cray/pe/modules/3.2.10.5/init/sh
    target=gaea
elif [[ -d /discover ]] ; then
    if [[ -d /usr/local/sgi/mpi ]] ; then
      target=discover
    else
      target=discover_opa
    fi
    build_type=0
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

if [ $target = wcoss_d ]; then
    module purge
    module use -a $dir_modules
    module load modulefile.ProdGSI.$target
elif [ $target = wcoss -o $target = gaea ]; then
    module purge
    module load $dir_modules/modulefile.ProdGSI.$target
elif [ $target = theia -o $target = cheyenne ]; then
    module purge
    source $dir_modules/modulefile.ProdGSI.$target
elif [ $target = wcoss_c ]; then
    module purge
    module load $dir_modules/modulefile.ProdGSI.$target
elif [ $target = discover ]; then
    module purge
    module use -a $dir_modules
    module load $dir_modules/modulefile.ProdGSI.$target
elif [ $target = discover_opa ]; then
    module purge
    module use -a $dir_modules
    module load modulefile.ProdGSI.$target
else 
    module purge
    source $dir_modules/modulefile.ProdGSI.$target
fi

if [ $build_type = PRODUCTION -o $build_type = DEBUG ] ; then
  cmake -DBUILD_UTIL=ON -DCMAKE_BUILD_TYPE=$build_type -DBUILD_CORELIBS=OFF ..
else 
  cmake ..
fi

make -j 8

exit
