#/bin/bash

# Build and run script
cd app/

if ghc -o sa ConnectDB.hs FunctionsDB.hs Main.hs; then
    cd ..
    ./app/sa
fi
