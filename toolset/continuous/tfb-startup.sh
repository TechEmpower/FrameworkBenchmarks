#!/bin/bash

set -e

echo "running tfb-shutdown script"
./tfb-shutdown.sh

echo "removing old tfb directory if necessary"
if [ -d "$TFB_REPOPARENT/$TFB_REPONAME" ]; then
  sudo rm -rf $TFB_REPOPARENT/$TFB_REPONAME
fi

echo "cloning tfb repository"
git clone \
  -b $TFB_REPOBRANCH \
  $TFB_REPOURI \
  $TFB_REPOPARENT/$TFB_REPONAME \
  --depth 1

echo "moving to tfb directory"
cd $TFB_REPOPARENT/$TFB_REPONAME

if [ -z "$TFB_RUN_ORDER" ]; then
  export TFB_RUN_ORDER="reverse"
else
  unset TFB_RUN_ORDER
fi

echo "building tfb docker image"
docker build -t techempower/tfb \
  --build-arg USER_ID=$(id -u) \
  --build-arg GROUP_ID=$(id -g) .

echo "running tfb docker image"
docker run \
  -e USER_ID=$(id -u) \
  --network=host \
  --mount type=bind,source=$TFB_REPOPARENT/$TFB_REPONAME,target=/FrameworkBenchmarks \
  techempower/tfb \
  --server-host $TFB_SERVER_HOST \
  --client-host $TFB_CLIENT_HOST \
  --database-host $TFB_DATABASE_HOST \
  --network-mode host \
  --results-name "$TFB_RUN_NAME" \
  --results-environment "$TFB_ENVIRONMENT" \
  --results-upload-uri "$TFB_UPLOAD_URI" \
  $(if [ "$TFB_RUN_ORDER" = "reverse" ]; then echo "--reverse-order"; fi) \
  --quiet

echo "zipping the results"
zip -r results.zip results

echo "uploading the results"
curl \
  -i -v \
  -X POST \
  --header "Content-Type: application/zip" \
  --data-binary @results.zip \
  $TFB_UPLOAD_URI

echo "done uploading results"
