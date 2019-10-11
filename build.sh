#!/bin/sh

set -ex;

mkdir -p deft
cp ~/Dropbox/conf/pandoc/gothic.css .
for f in ~/Dropbox/Deft/topology.txt
do
    pandoc --standalone --mathjax -f markdown -t html \
           --metadata pagetitle="$f" \
           --css https://k-bx.github.io/gothic.css \
           $f -o ./deft/$(basename ${f%.*}).html
done
