#!/bin/bash
echo 'setup-web2py-nginx-uwsgi-ubuntu-precise.sh'
echo 'Requires Ubuntu > 12.04 and installs Nginx + uWSGI + Web2py'
# Check if user has root privileges
if [[ $EUID -ne 0 ]]; then
   echo "You must run the script as root or using sudo"
   exit 1
fi
# Get Web2py Admin Password
echo -e "Web2py Admin Password: \c "
read  PW
# Upgrade and install needed software
apt-get update
apt-get -y upgrade
apt-get autoremove
apt-get autoclean
apt-get -y install nginx-full
apt-get -y install build-essential python-dev libxml2-dev python-pip unzip
pip install setuptools --no-use-wheel --upgrade
PIPPATH=`which pip`
$PIPPATH install --upgrade uwsgi
# Create common nginx sections
mkdir /etc/nginx/conf.d/web2py
echo '
gzip_static on;
gzip_http_version   1.1;
gzip_proxied        expired no-cache no-store private auth;
gzip_disable        "MSIE [1-6]\.";
gzip_vary           on;
' > /etc/nginx/conf.d/web2py/gzip_static.conf
echo '
gzip on;
gzip_disable "msie6";
gzip_vary on;
gzip_proxied any;
gzip_comp_level 6;
gzip_buffers 16 8k;
gzip_http_version 1.1;
gzip_types text/plain text/css application/json application/x-javascript text/xml application/xml application/xml+rss text/javascript;
' > /etc/nginx/conf.d/web2py/gzip.conf
# Create configuration file /etc/nginx/sites-available/web2py
echo 'server {
        listen          80;
        server_name     $hostname;
        ###to enable correct use of response.static_version
        location ~* ^/(\w+)/static(?:/_[\d]+\.[\d]+\.[\d]+)?/(.*)$ {
            alias /home/www-data/web2py/applications/$1/static/$2;
            expires max;
            ### if you want to use pre-gzipped static files (recommended)
            ### check scripts/zip_static_files.py and remove the comments
            # include /etc/nginx/conf.d/web2py/gzip_static.conf;
        }
        ###

        ###if you use something like myapp = dict(languages=['en', 'it', 'jp'], default_language='en') in your routes.py
        #location ~* ^/(\w+)/(en|it|jp)/static/(.*)$ {
        #    alias /home/www-data/web2py/applications/$1/;
        #    try_files static/$2/$3 static/$3 =404;
        #}
        ###
        
        location / {
            #uwsgi_pass      127.0.0.1:9001;
            uwsgi_pass      unix:///tmp/web2py.socket;
            include         uwsgi_params;
            uwsgi_param     UWSGI_SCHEME $scheme;
            uwsgi_param     SERVER_SOFTWARE    nginx/$nginx_version;

            ###remove the comments to turn on if you want gzip compression of your pages
            # include /etc/nginx/conf.d/web2py/gzip.conf;
            ### end gzip section

            ### remove the comments if you use uploads (max 10 MB)
            #client_max_body_size 10m;
            ###
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
        location / {
            #uwsgi_pass      127.0.0.1:9001;
            uwsgi_pass      unix:///tmp/web2py.socket;
            include         uwsgi_params;
            uwsgi_param     UWSGI_SCHEME $scheme;
            uwsgi_param     SERVER_SOFTWARE    nginx/$nginx_version;
            ###remove the comments to turn on if you want gzip compression of your pages
            # include /etc/nginx/conf.d/web2py/gzip.conf;
            ### end gzip section
            ### remove the comments if you want to enable uploads (max 10 MB)
            #client_max_body_size 10m;
            ###
        }
        ###to enable correct use of response.static_version
        location ~* ^/(\w+)/static(?:/_[\d]+\.[\d]+\.[\d]+)?/(.*)$ {
            alias /home/www-data/web2py/applications/$1/static/$2;
            expires max;
            ### if you want to use pre-gzipped static files (recommended)
            ### check scripts/zip_static_files.py and remove the comments
            # include /etc/nginx/conf.d/web2py/gzip_static.conf;
        }
        ###

}' >/etc/nginx/sites-available/web2py

ln -s /etc/nginx/sites-available/web2py /etc/nginx/sites-enabled/web2py
rm /etc/nginx/sites-enabled/default
mkdir /etc/nginx/ssl
cd /etc/nginx/ssl

openssl genrsa 1024 > web2py.key
chmod 400 web2py.key
openssl req -new -x509 -nodes -sha1 -days 1780 -key web2py.key > web2py.crt
openssl x509 -noout -fingerprint -text < web2py.crt > web2py.info


# Prepare folders for uwsgi
sudo mkdir /etc/uwsgi
sudo mkdir /var/log/uwsgi

# Create configuration file /etc/uwsgi/web2py.ini
echo '[uwsgi]

socket = /tmp/web2py.socket
pythonpath = /home/www-data/web2py/
mount = /=wsgihandler:application
processes = 4
master = true
harakiri = 60
reload-mercy = 8
cpu-affinity = 1
stats = /tmp/stats.socket
max-requests = 2000
limit-as = 512
reload-on-as = 256
reload-on-rss = 192
uid = www-data
gid = www-data
touch-reload = /home/www-data/web2py/routes.py
cron = 0 0 -1 -1 -1 python /home/www-data/web2py/web2py.py -Q -S welcome -M -R scripts/sessions2trash.py -A -o
no-orphans = true
' >/etc/uwsgi/web2py.ini

#Create a configuration file for uwsgi in emperor-mode
#for Upstart in /etc/init/uwsgi-emperor.conf
echo '# Emperor uWSGI script

description "uWSGI Emperor"
start on runlevel [2345]
stop on runlevel [06]
##
#remove the comments in the next section to enable static file compression for the welcome app
#in that case, turn on gzip_static on; on /etc/nginx/nginx.conf
##
#pre-start script
#    python /home/www-data/web2py/web2py.py -S welcome -R scripts/zip_static_files.py
#    chown -R www-data:www-data /home/www-data/web2py/*
#end script
respawn
exec uwsgi --master --die-on-term --emperor /etc/uwsgi --logto /var/log/uwsgi/uwsgi.log
' > /etc/init/uwsgi-emperor.conf
# Install Web2py
mkdir /home/www-data
cd /home/www-data
wget http://web2py.com/examples/static/web2py_src.zip
unzip web2py_src.zip
mv web2py/handlers/wsgihandler.py web2py/wsgihandler.py
rm web2py_src.zip
chown -R www-data:www-data web2py
cd /home/www-data/web2py
sudo -u www-data python -c "from gluon.main import save_password; save_password('$PW',443)"
start uwsgi-emperor
/etc/init.d/nginx restart

## you can reload uwsgi with
# restart uwsgi-emperor
## and stop it with
# stop uwsgi-emperor
## to reload web2py only (without restarting uwsgi)
# touch /etc/uwsgi/web2py.ini
