#!/bin/bash
set -e

cd "${0%/*}"

AGENT_FILE=$2
LANG=$1
YAML_TEMPLATE=contrast_security-template.yaml
YAML_FILE=contrast_security.yaml

if [[ ! -f $AGENT_FILE ]];
    then echo "Could not find $AGENT_FILE" && EXIT_NOW=true;
fi;

if [[ -z $CONTRAST__API__URL ]];
    then echo "CONTRAST__API__URL is not set" && EXIT_NOW=true;
fi;

if [[ -z $CONTRAST__API__API_KEY ]];
    then echo "CONTRAST__API__API_KEY is not set" && EXIT_NOW=true;
fi;

if [[ -z $CONTRAST__API__SERVICE_KEY ]];
    then echo "CONTRAST__API__SERVICE_KEY is not set" && EXIT_NOW=true;
fi;

if [[ -z $CONTRAST__API__USER_NAME ]];
    then echo "CONTRAST__API__USER_NAME is not set" && EXIT_NOW=true;
fi;

if [[ $EXIT_NOW ]];
    then exit 1;
fi;

if [[ $LANG == 'Go' ]];
then 
    GRPC='true';
else
    GRPC='false';
fi;

# Replace templated strings that are common to all configs for this agent
sed "s#\$LANG#$1#; s#\$URL#$CONTRAST__API__URL#; s#\$API_KEY#$CONTRAST__API__API_KEY#; s#\$SERVICE_KEY#$CONTRAST__API__SERVICE_KEY#; s#\$USER_NAME#$CONTRAST__API__USER_NAME#; s#\$GRPC#$GRPC#;" $YAML_TEMPLATE > $YAML_FILE

# Copy files into correct place
ls -d ../frameworks/$LANG/*/ | xargs -n 1 cp -v $YAML_FILE $AGENT_FILE

# Replace $FRAMEWORK template string
ls -d ../frameworks/$LANG/*/ | sed "s#../frameworks/$LANG/##; s#/##;" | xargs -I '{}' -n 1 sed -i.bak "s#\$FRAMEWORK#{}#;" ../frameworks/$LANG/{}/$YAML_FILE && ls -d ../frameworks/$LANG/*/*.bak | xargs -n 1 rm
