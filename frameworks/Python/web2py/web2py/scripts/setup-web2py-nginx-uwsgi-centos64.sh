#!/bin/bash
# Autor: Nilton OS -- www.linuxpro.com.br
echo 'setup-web2py-nginx-uwsgi-centos64.sh'
echo 'Support CentOS  6.4'
echo 'Installs Nginx 1.4.1 + uWSGI + Web2py'


# Get Web2py Admin Password
echo -e "Web2py Admin Password: \c "
read  PW

echo -e "Set Server Name Ex: web2py.domain.com : \c "
read  SERVER_FQDN

echo -e "Set Server IP: \c "
read  SERVER_IP


echo "" >>/etc/hosts
echo "$SERVER_IP  $SERVER_FQDN" >>/etc/hosts

yum update -y

yum install -y http://mirror-fpt-telecom.fpt.net/fedora/epel/6/i386/epel-release-6-8.noarch.rpm
yum clean all
yum install -y gcc libxml2-devel python-devel python-pip PyXML unzip make sudo

## 64Bits System
## yum install -y http://nginx.org/packages/rhel/6/x86_64/RPMS/nginx-1.4.1-1.el6.ngx.x86_64.rpm
yum install -y http://nginx.org/packages/rhel/6/i386/RPMS/nginx-1.4.1-1.el6.ngx.i386.rpm


pip-python install --upgrade pip
PIPPATH=`which pip`
$PIPPATH install --upgrade uwsgi


# Prepare folders for uwsgi
mkdir /etc/uwsgi
mkdir /var/log/uwsgi
mkdir -p /var/www/

#usermod -a -G apache nginx
mkdir -p /etc/nginx/ssl/


cd /etc/nginx/ssl
openssl genrsa 1024 > web2py.key && chmod 400 web2py.key
openssl req -new -x509 -nodes -sha1 -days 1780 -key web2py.key > web2py.crt
openssl x509 -noout -fingerprint -text < web2py.crt > web2py.info


echo 'server {
        listen          YOUR_SERVER_IP:80;
        server_name     YOUR_SERVER_FQDN;
        #to enable correct use of response.static_version
        location ~* /(\w+)/static(?:/_[\d]+\.[\d]+\.[\d]+)?/(.*)$ {
            alias /var/www/web2py/applications/$1/static/$2;
            expires max;
        }
        location / {
            #uwsgi_pass      127.0.0.1:9001;
            uwsgi_pass      unix:///var/www/web2py/logs/web2py.socket;
            include         /etc/nginx/uwsgi_params;
            uwsgi_param     UWSGI_SCHEME $scheme;
            uwsgi_param     SERVER_SOFTWARE    nginx/$nginx_version;

            ### remove the comments if you use uploads (max 10 MB)
            #client_max_body_size 10m;
            ###
        }
}
server {
        listen YOUR_SERVER_IP:443 default_server ssl;
        server_name     YOUR_SERVER_FQDN;
        ssl_certificate         /etc/nginx/ssl/web2py.crt;
        ssl_certificate_key     /etc/nginx/ssl/web2py.key;
        ssl_prefer_server_ciphers on;
        ssl_session_cache shared:SSL:10m;
        ssl_session_timeout 10m;
        ssl_ciphers ECDHE-RSA-AES256-SHA:DHE-RSA-AES256-SHA:DHE-DSS-AES256-SHA:DHE-RSA-AES128-SHA:DHE-DSS-AES128-SHA;
        ssl_protocols SSLv3 TLSv1;
        keepalive_timeout    70;
        location ~* /(\w+)/static(?:/_[\d]+\.[\d]+\.[\d]+)?/(.*)$ {
            alias /var/www/web2py/applications/$1/static/$2;
            expires max;
        }
        location / {
            #uwsgi_pass      127.0.0.1:9001;
            uwsgi_pass      unix:///var/www/web2py/logs/web2py.socket;
            include         /etc/nginx/uwsgi_params;
            uwsgi_param     UWSGI_SCHEME $scheme;
            uwsgi_param     SERVER_SOFTWARE    nginx/$nginx_version;

            ### remove the comments if you use uploads (max 10 MB)
            #client_max_body_size 10m;
            ###
        }

}' >/etc/nginx/conf.d/web2py.conf

sed -i "s/YOUR_SERVER_IP/$SERVER_IP/" /etc/nginx/conf.d/web2py.conf
sed -i "s/YOUR_SERVER_FQDN/$SERVER_FQDN/" /etc/nginx/conf.d/web2py.conf


# Create configuration file /etc/uwsgi/web2py.ini
echo '[uwsgi]

socket = /var/www/web2py/logs/%n.socket
pythonpath = /var/www/web2py/
mount = /=wsgihandler:application
processes = 4
master = true
harakiri = 60
reload-mercy = 8
cpu-affinity = 1
stats = /tmp/%n.stats.socket
max-requests = 2000
limit-as = 512
reload-on-as = 256
reload-on-rss = 192
uid = nginx
gid = nginx
cron = 0 0 -1 -1 -1 python /var/www/web2py/web2py.py -Q -S welcome -M -R scripts/sessions2trash.py -A -o
no-orphans = true
chmod-socket = 666
' >/etc/uwsgi/web2py.ini


cd /var/www/
curl --progress -O http://web2py.com/examples/static/web2py_src.zip
unzip web2py_src.zip && rm -rf web2py_src.zip
# Download latest version of sessions2trash.py
mv web2py/handlers/wsgihandler.py web2py/wsgihandler.py
chown -R nginx:nginx web2py
cd /var/www/web2py
sudo -u nginx python -c "from gluon.main import save_password; save_password('$PW',443)"



## Daemons /start/stop

echo '#!/bin/sh
# Autor: Nilton OS -- www.linuxpro.com.br
#
#
### BEGIN INIT INFO
# Provides:          uwsgi
# Required-Start:    $syslog $remote_fs
# Should-Start:      $time ypbind smtp
# Required-Stop:     $syslog $remote_fs
# Should-Stop:       ypbind smtp
# Default-Start:     3 5
# Default-Stop:      0 1 2 6
### END INIT INFO

# Source function library.
. /etc/rc.d/init.d/functions

# Check for missing binaries (stale symlinks should not happen)
UWSGI_BIN=`which uwsgi`
test -x $UWSGI_BIN || { echo "$UWSGI_BIN not installed";
        if [ "$1" = "stop" ]; then exit 0;
        else exit 5; fi; }

UWSGI_EMPEROR_MODE=true
UWSGI_VASSALS="/etc/uwsgi/"
UWSGI_OPTIONS="--enable-threads --logto /var/log/uwsgi/uwsgi.log"
lockfile=/var/lock/subsys/uwsgi

UWSGI_OPTIONS="$UWSGI_OPTIONS --autoload"

if [ "$UWSGI_EMPEROR_MODE" = "true" ] ; then
    UWSGI_OPTIONS="$UWSGI_OPTIONS --emperor $UWSGI_VASSALS"
fi

case "$1" in
    start)
        echo -n "Starting uWSGI "
        daemon $UWSGI_BIN $UWSGI_OPTIONS &
        ;;
    stop)
        echo -n "Shutting down uWSGI "
        killproc $UWSGI_BIN
        ;;
    restart)
        $0 stop
        $0 start
        ;;
    status)
        echo -n "Checking for service uWSGI "
        status $UWSGI_BIN
        ;;
    *)
        echo "Usage: $0 {start|stop|status|restart}"
        exit 1
        ;;
esac
exit 0 '> /etc/init.d/uwsgi

chmod +x /etc/init.d/uwsgi

/etc/init.d/uwsgi start
/etc/init.d/nginx start

/etc/init.d/iptables stop
chkconfig --del iptables

chkconfig --levels 235 uwsgi on
chkconfig --levels 235 nginx on

## you can reload uwsgi with
#/etc/init.d/uwsgi restart
## to reload web2py only (without restarting uwsgi)
# touch /etc/uwsgi/web2py.ini
