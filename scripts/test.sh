#! /bin/bash

cd ./test/scanner
cd ../compiler
./scripts/build.sh > build.log
./scripts/test.sh
./scripts/clean.sh