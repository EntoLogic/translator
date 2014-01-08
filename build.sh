#!/bin/bash

[[ $CBDV != "" ]] || CBDV="cabal-dev"

cd translator
$CBDV add-source ..
$CBDV install entologic-lib --reinstall
$CBDV install
cd ..

