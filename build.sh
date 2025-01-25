#/bin/bash

# Build and run script
cd build/

if ghc -o sa ../app/ConnectDB.hs ../app/FunctionsDB.hs ../app/Main.hs; then
    cd ..
    ./app/sa
fi
