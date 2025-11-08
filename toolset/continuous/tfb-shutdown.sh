#!/bin/bash

set -e

docker_clean() {
  echo "restarting docker"
  sudo service docker restart

  echo "running 'docker stop'"
  docker ps --all --quiet | xargs --no-run-if-empty docker stop

  echo "running 'docker rm'"
  docker ps --all --quiet | xargs --no-run-if-empty docker rm --force

  echo "checking disk space"
  # https://stackoverflow.com/a/38183298/359008
  FREE=`df -k --output=avail /var/lib/docker | tail -n1`   # df -k not df -h
  if [[ $FREE -lt 52428800 ]]; then                        # 50G = 50*1024*1024k
    echo "running 'docker system prune'"
    docker system prune --all --force
  fi
}

echo "running docker_clean on server host"
docker_clean

echo "running docker_clean on database host"
ssh techempower@$TFB_DATABASE_HOST "$(typeset -f docker_clean); docker_clean"

echo "running docker_clean on client host"
ssh techempower@$TFB_CLIENT_HOST "$(typeset -f docker_clean); docker_clean"

echo "done with tfb-shutdown script"
