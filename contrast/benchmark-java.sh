#!/bin/bash

cd "${0%/*}"

# Use files in the script directory by default
JAR_FILE=${1-contrast.jar}
YAML_FILE=${2-contrast_security.yaml}

if [[ $(basename -- "$JAR_FILE") != "contrast.jar" ]];
    then echo "First argument must be a path to contrast.jar" && exit 1;
fi;

if [[ $(basename -- "$YAML_FILE") != "contrast_security.yaml" ]];
    then echo "Second argument must be a path to contrast_security.yaml" && exit 1;
fi;

if [[ ! -f $JAR_FILE ]];
    then echo "Could not find $JAR_FILE" && exit 1;
fi;

if [[ ! -f $YAML_FILE ]];
    then echo "Could not find $YAML_FILE" && exit 1;
fi;

echo "Using $JAR_FILE and $YAML_FILE"

# Copy files into correct place
ls -d ../frameworks/Java/*/ | xargs -n 1 cp $JAR_FILE $YAML_FILE

../tfb --tag contrast --test-lang Java --type fortune --duration 60
