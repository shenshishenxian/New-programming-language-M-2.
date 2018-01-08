cd ./test/scanner
./scripts/build.sh
./scripts/test.sh
./scripts/clean.sh
cd ../parser
./scripts/test.sh
./scripts/clean.sh
cd ../compiler
./scripts/build.sh > build.log
./scripts/test.sh
./scripts/clean.sh