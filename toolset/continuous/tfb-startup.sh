#!/bin/bash

set -e

while true
do
  ./tfb-shutdown.sh

  git clone \
    -b $TFB_REPOBRANCH \
    $TFB_REPOURI \
    $TFB_REPOPARENT/$TFB_REPONAME \
    --depth 1

  cd $TFB_REPOPARENT/$TFB_REPONAME

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
    --results-upload-uri "$TFB_UPLOAD_URI"

  zip -r results.zip results

  curl \
    -i -v \
    -X POST \
    --header "Content-Type: application/zip" \
    --data-binary @results.zip \
    $TFB_UPLOAD_URI

  sleep 5
done
