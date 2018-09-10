#!/bin/sh

mkdir new

set -e
cp -p *sh new/
for f in *.sh
do
  echo "processing $f"
  sed 's/ptmp/ptmpp1/g' $f >$f.new
  mv $f.new $f
done
