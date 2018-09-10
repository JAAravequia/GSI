#!/bin/sh

mkdir old 

set -e
cp -p *sh old/
for f in *.sh
do
  echo "processing $f"
  sed 's/ptmpp1/ptmpp1p1/g' $f >$f.new
  mv $f.new $f
done
