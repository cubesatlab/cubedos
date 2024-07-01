#!/bin/bash

# Get the directory where the script is located and use that as a base for the paths below.
SCRIPT_DIR=$(dirname $(realpath $0))

java -jar $SCRIPT_DIR/../mercury/target/scala-3.4.2/Mercury-assembly-0.1.0-SNAPSHOT.jar \
            -t$SCRIPT_DIR/../mercury/templates $*
