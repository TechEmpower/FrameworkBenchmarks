#!/bin/bash

# Stop any leftover docker containers on this server and on the
# database server, which might not have been cleaned up if the
# previous run was forcibly stopped.
docker stop $(docker ps -aq)
ssh techempower@$TFB_DATABASE_HOST << \EOF
  docker stop $(docker ps -aq)
EOF
ssh techempower@$TFB_CLIENT_HOST << \EOF
  docker stop $(docker ps -aq)
EOF

while true
do
  cd $TFB_REPOPARENT
  if [ -d "$TFB_REPOPARENT/$TFB_REPONAME" ]; then
    sudo rm -rf $TFB_REPOPARENT/$TFB_REPONAME
  fi
  git clone -b $TFB_REPOBRANCH $TFB_REPOURI $TFB_REPOPARENT/$TFB_REPONAME --depth 1
  cd $TFB_REPOPARENT/$TFB_REPONAME

  docker run \
    --network=host \
    --mount type=bind,source=$TFB_REPOPARENT/$TFB_REPONAME,target=/FrameworkBenchmarks \
    techempower/tfb \
    --server-host $TFB_SERVER_HOST \
    --client-host $TFB_CLIENT_HOST \
    --database-host $TFB_DATABASE_HOST \
    --network-mode host \
    --clean

  docker run \
    --network=host \
    --mount type=bind,source=$TFB_REPOPARENT/$TFB_REPONAME,target=/FrameworkBenchmarks \
    techempower/tfb \
    --server-host $TFB_SERVER_HOST \
    --client-host $TFB_CLIENT_HOST \
    --database-host $TFB_DATABASE_HOST \
    --network-mode host \
    --results-name "$TFB_RESULTS_NAME" \
    --results-environment "$TFB_ENVIRONMENT_NAME" \
    --results-upload-uri "$TFB_UPLOAD_URI" \
    --quiet

  zip -r $TFB_REPOPARENT/$TFB_REPONAME/results.zip $TFB_REPOPARENT/$TFB_REPONAME/results
  curl -i -v -X POST --header "Content-Type: application/zip" --data-binary @$TFB_REPOPARENT/$TFB_REPONAME/results.zip $TFB_UPLOAD_URI

  sleep 5
done
