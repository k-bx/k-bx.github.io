#!/bin/sh

set -ex;

mkdir -p deft
for f in ~/Dropbox/Deft/topology.txt
do
    pandoc --standalone --mathjax -f markdown -t html \
           --metadata pagetitle="$f" \
           --css ~/Dropbox/conf/pandoc/gothic.css \
           $f -o ./deft/$(basename ${f%.*}).html
done
