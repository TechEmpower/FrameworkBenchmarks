#!/bin/bash

set -e

docker_clean() {
  sudo service docker restart
  docker ps --all --quiet | xargs --no-run-if-empty docker stop
  docker ps --all --quiet | xargs --no-run-if-empty docker rm --force

  # https://stackoverflow.com/a/38183298/359008
  FREE=`df -k --output=avail /var/lib/docker | tail -n1`   # df -k not df -h
  if [[ $FREE -lt 20971520 ]]; then                        # 20G = 20*1024*1024k
    docker system prune --all --force
  fi
}

docker_clean
ssh techempower@$TFB_DATABASE_HOST "$(typeset -f docker_clean); docker_clean"
ssh techempower@$TFB_CLIENT_HOST "$(typeset -f docker_clean); docker_clean"
