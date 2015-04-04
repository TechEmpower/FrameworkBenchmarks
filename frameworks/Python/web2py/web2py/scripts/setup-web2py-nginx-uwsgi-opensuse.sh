#!/bin/bash
echo 'setup-web2py-nginx-uwsgi-opensuse.sh'
echo 'Requires OpenSUSE  12.3 32Bits and installs Nginx + uWSGI + Web2py'


# Get Web2py Admin Password
echo -e "Web2py Admin Password: \c "
read  PW

zypper clean && zypper refresh && zypper up
zypper in -y nginx python-xml python-pip unzip sudo
zypper in -y gcc python-devel libxml2-devel python-pip unzip
pip install --upgrade pip
PIPPATH=`which pip`
$PIPPATH install --upgrade uwsgi


# Prepare folders for uwsgi
mkdir /etc/uwsgi
mkdir /var/log/uwsgi

usermod -G www nginx

mkdir -p /etc/nginx/vhosts.d/
mkdir -p /etc/nginx/ssl/


cd /etc/nginx/ssl
openssl genrsa 1024 > web2py.key
chmod 400 web2py.key
openssl req -new -x509 -nodes -sha1 -days 1780 -key web2py.key > web2py.crt
openssl x509 -noout -fingerprint -text < web2py.crt > web2py.info


echo 'server {
        listen          80;
        server_name     $hostname;
        #to enable correct use of response.static_version
        location ~* /(\w+)/static(?:/_[\d]+\.[\d]+\.[\d]+)?/(.*)$ {
            alias /srv/www/web2py/applications/$1/static/$2;
            expires max;
        }
        location / {
            #uwsgi_pass      127.0.0.1:9001;
            uwsgi_pass      unix:///tmp/web2py.socket;
            include         /etc/nginx/uwsgi_params;
            uwsgi_param     UWSGI_SCHEME $scheme;
            uwsgi_param     SERVER_SOFTWARE    nginx/$nginx_version;
        }
}
server {
        listen 443 default_server ssl;
        server_name     $hostname;
        ssl_certificate         /etc/nginx/ssl/web2py.crt;
        ssl_certificate_key     /etc/nginx/ssl/web2py.key;
        ssl_prefer_server_ciphers on;
        ssl_session_cache shared:SSL:10m;
        ssl_session_timeout 10m;
        ssl_ciphers ECDHE-RSA-AES256-SHA:DHE-RSA-AES256-SHA:DHE-DSS-AES256-SHA:DHE-RSA-AES128-SHA:DHE-DSS-AES128-SHA;
        ssl_protocols SSLv3 TLSv1;
        keepalive_timeout    70;
        location ~* /(\w+)/static(?:/_[\d]+\.[\d]+\.[\d]+)?/(.*)$ {
            alias /srv/www/web2py/applications/$1/static/$2;
            expires max;
        }
        location / {
            #uwsgi_pass      127.0.0.1:9001;
            uwsgi_pass      unix:///tmp/web2py.socket;
            include         /etc/nginx/uwsgi_params;
            uwsgi_param     UWSGI_SCHEME $scheme;
            uwsgi_param     SERVER_SOFTWARE    nginx/$nginx_version;
        }

}' >/etc/nginx/vhosts.d/web2py.conf



# Create configuration file /etc/uwsgi/web2py.xml
echo '<uwsgi>
    <socket>/tmp/web2py.socket</socket>
    <pythonpath>/srv/www/web2py/</pythonpath>
    <mount>/=wsgihandler:application</mount>
    <master/>
    <processes>4</processes>
    <harakiri>60</harakiri>
    <reload-mercy>8</reload-mercy>
    <cpu-affinity>1</cpu-affinity>
    <stats>/tmp/stats.socket</stats>
    <max-requests>2000</max-requests>
    <limit-as>512</limit-as>
    <reload-on-as>256</reload-on-as>
    <reload-on-rss>192</reload-on-rss>
    <uid>nginx</uid>
    <gid>www</gid>
    <cron>0 0 -1 -1 -1 python /srv/www/web2py/web2py.py -Q -S welcome -M -R scripts/sessions2trash.py -A -o</cron>
    <no-orphans/>
</uwsgi>' >/etc/uwsgi/web2py.xml


cd /srv/www/
wget http://web2py.com/examples/static/web2py_src.zip
unzip web2py_src.zip
rm web2py_src.zip
mv web2py/handlers/wsgihandler.py web2py/wsgihandler.py
chown -R nginx:www web2py
cd /srv/www/web2py
sudo -u nginx python -c "from gluon.main import save_password; save_password('$PW',443)"



## Daemons /start/stop

echo '#!/bin/sh
# Copyright (c) 2012 SUSE LINUX Products GmbH, Nuernberg, Germany.
#
# Author: James Oakley
#
# /etc/init.d/uwsgi
#   and its symbolic link
# /(usr/)sbin/rcuwsgi
#
# LSB compatible service control script; see http://www.linuxbase.org/spec/
#
### BEGIN INIT INFO
# Provides:          uwsgi
# Required-Start:    $syslog $remote_fs
# Should-Start:      $time ypbind smtp
# Required-Stop:     $syslog $remote_fs
# Should-Stop:       ypbind smtp
# Default-Start:     3 5
# Default-Stop:      0 1 2 6
# Short-Description: Application Container Server for Networked/Clustered Web Applications
# Description:       Application Container Server for Networked/Clustered Web Applications
### END INIT INFO

# Check for missing binaries (stale symlinks should not happen)
UWSGI_BIN=/usr/bin/uwsgi
test -x $UWSGI_BIN || { echo "$UWSGI_BIN not installed";
        if [ "$1" = "stop" ]; then exit 0;
        else exit 5; fi; }

UWSGI_EMPEROR_MODE=true
UWSGI_VASSALS="/etc/uwsgi/"
UWSGI_OPTIONS="--logto /var/log/uwsgi/uwsgi.log"


UWSGI_OPTIONS="$UWSGI_OPTIONS --autoload"

if [ "$UWSGI_EMPEROR_MODE" = "true" ] ; then
    UWSGI_OPTIONS="$UWSGI_OPTIONS --emperor $UWSGI_VASSALS"
fi

. /etc/rc.status

rc_reset

case "$1" in
    start)
        echo -n "Starting uWSGI "
        /sbin/startproc $UWSGI_BIN $UWSGI_OPTIONS
        rc_status -v
        ;;
    stop)
        echo -n "Shutting down uWSGI "
        /sbin/killproc $UWSGI_BIN
        rc_status -v
        ;;
    try-restart|condrestart)
        if test "$1" = "condrestart"; then
                echo "${attn} Use try-restart ${done}(LSB)${attn} rather than condrestart ${warn}(RH)${norm}"
        fi
        $0 status
        if test $? = 0; then
                $0 restart
        else
                rc_reset
        fi
        rc_status
        ;;
    restart)
        $0 stop
        $0 start
        rc_status
        ;;
    force-reload)
        echo -n "Reload service uWSGI "
        /sbin/killproc -HUP $UWSGI_BIN
        rc_status -v
        ;;
    reload)
        echo -n "Reload service uWSGI "
        /sbin/killproc -HUP $UWSGI_BIN
        rc_status -v
        ;;
    status)
        echo -n "Checking for service uWSGI "
        /sbin/checkproc $UWSGI_BIN
        rc_status -v
        ;;
    probe)
        echo -n "uWSGI does not support probe "
        rc_failed 3
        rc_status -v
        ;;
    *)
        echo "Usage: $0 {start|stop|status|try-restart|restart|force-reload|reload|probe}"
        exit 1
        ;;
esac
rc_exit '> /etc/init.d/uwsgi


chmod +x /etc/init.d/uwsgi
/etc/init.d/uwsgi start
/etc/init.d/nginx restart


chkconfig --add uwsgi
chkconfig --add nginx

## you can reload uwsgi with
#/etc/init.d/uwsgi restart
## to reload web2py only (without restarting uwsgi)
# touch /etc/uwsgi/web2py.xml
