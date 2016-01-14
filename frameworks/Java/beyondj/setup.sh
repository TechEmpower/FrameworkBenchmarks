RKING_DIR=beyondj-launcher/deploy
FILE=beyondj-launcher/deploy/beyondj.jar

if [ ! -d "$WORKING_DIR" ]; then
        mkdir $WORKING_DIR
fi

#if [ ! -f "$FILE" ]; then
        cd beyondj-launcher/deploy/
        wget http://beyondj.com/beyondj.jar
        cd ../../
#fi

echo "Launching BeyondJ from location:$PWD"
java -jar beyondj-launcher/deploy/beyondj.jar system.platform.dbserver=${DBHOST} numInstances=10

