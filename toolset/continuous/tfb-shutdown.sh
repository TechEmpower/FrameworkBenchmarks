#!/bin/bash

set -e

docker ps -aq | xargs --no-run-if-empty docker stop
docker system prune -af
ssh techempower@$TFB_DATABASE_HOST << \EOF
  docker ps -aq | xargs --no-run-if-empty docker stop
  docker system prune -af
EOF
ssh techempower@$TFB_CLIENT_HOST << \EOF
  docker ps -aq | xargs --no-run-if-empty docker stop
  docker system prune -af
EOF

if [ -d "$TFB_REPOPARENT/$TFB_REPONAME" ]; then
  sudo rm -rf $TFB_REPOPARENT/$TFB_REPONAME
fi
