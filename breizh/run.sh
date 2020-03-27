#!/bin/bash
mkdir -p dist
cp breizh.ml dist
cd dist
ocamlopt breizh.ml
./a.out $* | convert - out.png