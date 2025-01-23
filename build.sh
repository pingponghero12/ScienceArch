#/bin/bash

# Build and run script
cd app/

if ghc -o sarch ConnectDB.hs FunctionsDB.hs Main.hs; then
    ./sa
fi
