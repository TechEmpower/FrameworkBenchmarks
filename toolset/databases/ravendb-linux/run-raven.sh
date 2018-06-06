#!/bin/bash
COMMAND="./Raven.Server"
export RAVEN_ServerUrl="http://$(hostname):8080"

if [ ! -z "$RAVEN_SETTINGS" ]; then
    echo "$RAVEN_SETTINGS" > settings.json
fi

if [ ! -z "$RAVEN_ARGS" ]; then
	COMMAND="$COMMAND ${RAVEN_ARGS}"
fi

eval $COMMAND &

function create_database () {
    DBNAME="$1"
    curl --fail --silent --show-error -X PUT \
        -d "{\"DatabaseName\":\"$DBNAME\",\"Settings\":{},\"Disabled\":false,\"Encrypted\":false,\"Topology\":{\"DynamicNodesDistribution\":false}}" \
        "$RAVEN_ServerUrl/admin/databases?name=$DBNAME&replicationFactor=1"
}

function get_import_uri () {
    DBNAME="$1"
    echo "$RAVEN_ServerUrl/databases/$DBNAME/admin/smuggler/import";
}

sleep 3

echo
echo "Importing data"

# import dbs
create_database "world"
curl --fail --silent --show-error -X GET "$(get_import_uri world)?file=/datadumps/TechEmpower-World.ravendbdump"

create_database "fortunes"
curl --fail --silent --show-error -X GET "$(get_import_uri fortunes)?file=/datadumps/TechEmpower-Fortunes.ravendbdump"

echo
echo "Data imported"

while true; do
    ALIVE=`pgrep Raven.Server`;
    if [ -z "$ALIVE" ]; then
        exit 1;
    fi

    sleep 5;
done
