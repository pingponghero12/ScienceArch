#/bin/bash
cd app/
ghc -o sarch ConnectDB.hs FunctionsDB.hs Main.hs
./app/sarch
