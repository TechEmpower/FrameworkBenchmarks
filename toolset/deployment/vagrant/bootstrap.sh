#!/usr/bin/env bash
if [ ! -e "/home/vagrant/.firstboot" ]; then
  echo "ListenAddress 0.0.0.0" >> /etc/ssh/sshd_config
  service ssh restart
  touch /home/vagrant/.firstboot
fi

