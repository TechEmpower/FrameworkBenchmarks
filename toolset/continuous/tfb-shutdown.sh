#!/bin/bash

set -e

sudo service docker restart
docker ps -aq | xargs --no-run-if-empty docker stop
docker ps -aq | xargs --no-run-if-empty docker rm -f
ssh techempower@$TFB_DATABASE_HOST << \EOF
  sudo service docker restart
  docker ps -aq | xargs --no-run-if-empty docker stop
  docker ps -aq | xargs --no-run-if-empty docker rm -f
EOF
ssh techempower@$TFB_CLIENT_HOST << \EOF
  sudo service docker restart
  docker ps -aq | xargs --no-run-if-empty docker stop
  docker ps -aq | xargs --no-run-if-empty docker rm -f
EOF
