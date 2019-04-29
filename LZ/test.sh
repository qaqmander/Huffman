#!/bin/sh

if [ "$1" -gt "10000" ]; then
    python .fake.py $1
else 
    python gen.py $1
    cd src
    ./test
    cd ..
    wc mes*
fi
