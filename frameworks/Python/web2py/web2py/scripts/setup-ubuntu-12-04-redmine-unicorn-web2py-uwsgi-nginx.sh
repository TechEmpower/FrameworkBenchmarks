#!/bin/bash

# ------------------------------------------------------------------------------
# Description : Installation and basic configuration of web2py, uWSGI, Redmine,
#               Unicorn, Nginx and PostgreSQL.
#       Usage : Copy the script in /home/username and run it as root, you may
#               need to allow exectuion (chmod +x). Ex.:
#               sudo ./setup-ubuntu-12-04-redmine-unicorn-web2py-uwsgi-nginx.sh
#        File : setup-ubuntu-12-04-redmine-unicorn-web2py-uwsgi-nginx.sh
#      Author : Richard V?zina
#       Email : ml.richard.vezina@gmail.com
#   Copyright : Richard V?zina
#        Date : ven 28 d?c 2012 13:27:11 EST
# Disclaimers : This script is provided "as is", without warranty of any kind.
#     Licence : CC BY-NC 2.5 CA
# ------------------------------------------------------------------------------

echo 'setup-ubuntu-12-04-redmine-unicorn-web2py-uwsgi-nginx.sh'
echo 'Requires Ubuntu = 12.04 (May works with 12.10 not tested) and installs Redmine + Unicorn + Web2py + uWSGI + Nginx + PostgreSQL'
# Check if user has root privileges
if [[ $EUID -ne 0 ]]; then
   echo "You must run the script as root or using sudo"
   exit 1
fi

# ------------------------------------------------------------------------------
# We concentrate here user prompts!!
# Get Redmine Postgres Database Password
echo -e "Redmine Postgres Database Password: \c "
read  REDMINEPASSWORD
# Get Web2py Admin Password
echo -e "Web2py Admin Password: \c "
read  PW

cd ~
openssl genrsa 1024 > self_signed.key
chmod 400 self_signed.key
openssl req -new -x509 -nodes -sha1 -days 1780 -key self_signed.key > self_signed.cert
openssl x509 -noout -fingerprint -text < self_signed.cert > self_signed.info
# ------------------------------------------------------------------------------

apt-get update
apt-get -y upgrade
apt-get autoremove
apt-get autoclean
apt-get -y install postgresql
apt-get -y install nginx-full
apt-get -y install build-essential python-dev libxml2-dev python-pip unzip
apt-get -y install ruby1.9.3 # Ref.: http://askubuntu.com/questions/137485/rails-3-not-using-rvm
apt-get -y install libpq-dev # Required for gem1.9.3 install pg Ref.: http://stackoverflow.com/questions/6040583/unable-to-install-pg-gem-on-ubuntu-cant-find-the-libpq-fe-h-header

gem1.9.3 install rails --no-rdoc --no-ri # For testing (faster) --no-rdoc --no-ri
gem1.9.3 install unicorn --no-rdoc --no-ri # For testing (faster) --no-rdoc --no-ri
gem1.9.3 install pg --no-rdoc --no-ri # For testing (faster) --no-rdoc --no-ri
cd /opt
wget http://rubyforge.org/frs/download.php/76627/redmine-2.2.0.tar.gz
wget http://rubyforge.org/frs/download.php/76628/redmine-2.2.0.tar.gz.md5
md5sum --check redmine-2.2.0.tar.gz.md5 > redmine_md5_checked_successfully
if [ -f redmine_md5_checked_successfully ]
then
        tar xvfz redmine-2.2.0.tar.gz
        rm redmine_md5_checked_successfully
else
    echo "Redmine md5 check sum failed..."
    exit 1
fi
cd redmine-2.2.0
bundle install --without development test rmagick sqlite mysql
mkdir /var/www
ln -s /opt/redmine-2.2.0/public /var/www/redmine
chown -R www-data.www-data /var/www
chown -R www-data.www-data /opt/redmine-2.2.0/public
# To avoid prompt during execution of the script use psql instead of createuser
#echo "Enter a postgres redmine user password twice:"
#createuser -P -S -D -R -l -e redmine
# createuser switch: -P --pwprompt -S --no-superuser -D --no-createdb  -R --no-createrole -l --login -e --echo
sudo -u postgres psql -c "CREATE ROLE redmine LOGIN; ALTER ROLE redmine WITH ENCRYPTED PASSWORD '$REDMINEPASSWORD';"
# createdb wouldn't work without having root password
#createdb -U postgres -w -E UTF8 -O redmine -e redmine
# createdb switch: -U username --username=username -w --no-password -E Encoding -O owner --owner=owner -e --echo
sudo -u postgres psql -c "CREATE DATABASE redmine WITH ENCODING='UTF8' OWNER=redmine;"
cd /opt/redmine-2.2.0/config
# Here we change related to an issue with new rails version as far as I understand
# Ref1.: http://www.redmine.org/projects/redmine/wiki/HowTo_Install_Redmine_in_a_sub-URI # Preferred solution used
# Ref2.: http://www.redmine.org/issues/12102 # JS and CSS was not working until I add this line 'RedmineApp::Application.routes.default_scope =  { :path => "/redmine", :shallow_path => "/redmine" }' before 'RedmineApp::Application.initialize!'
cp environment.rb environment.rb_original # Backup default environment.rb
sed '/RedmineApp::Application.initialize!/c \RedmineApp::Application.routes.default_scope =  { :path => "/redmine", :shallow_path => "/redmine" }\nRedmineApp::Application.initialize!\nRedmine::Utils::relative_url_root = "/redmine"' environment.rb_original > environment.rb
# Now we configure Redmine database access
#nano database.yml
# paste :
echo 'production:
 adapter: postgresql
 database: redmine
 host: localhost
 username: redmine
 password: "'$REDMINEPASSWORD'"
 encoding: utf8' > database.yml
rake generate_secret_token
RAILS_ENV=production rake db:migrate
RAILS_ENV=production rake redmine:load_default_data
mkdir /opt/redmine-2.2.0/tmp/pids
#mkdir /opt/redmine-2.2.0/log # if not there
cd /opt/redmine-2.2.0/config
# Create Unicorn specific Redmine config in /opt/redmine-2.2.0/config/unicorn.rb
echo '#unicorn.rb Starts here
worker_processes 1
working_directory "/opt/redmine-2.2.0" # needs to be the correct directory for redmine

# This loads the application in the master process before forking
# worker processes
# Read more about it here:
# http://unicorn.bogomips.org/Unicorn/Configurator.html
preload_app true
timeout 45

# This is where we specify the socket.
# We will point the upstream Nginx module to this socket later on
listen "/tmp/unicorn_rails.socket", :backlog => 64 #directory structure needs to be created.
pid "/opt/redmine-2.2.0/tmp/pids/unicorn_rails.pid" # make sure this points to a valid directory. Make sure it is named the same as the real process name in order to allow init.d script start-stop-daemon command to kill unicorn process properly

# Set the path of the log files inside the log folder of the testapp
stderr_path "/opt/redmine-2.2.0/log/unicorn_rails.stderr.log"
stdout_path "/opt/redmine-2.2.0/log/unicorn_rails.stdout.log"

before_fork do |server, worker|
# This option works in together with preload_app true setting
# What is does is prevent the master process from holding
# the database connection
defined?(ActiveRecord::Base) and
ActiveRecord::Base.connection.disconnect!
end

after_fork do |server, worker|
# Here we are establishing the connection after forking worker
# processes
defined?(ActiveRecord::Base) and
ActiveRecord::Base.establish_connection
# change below if your redmine instance is running differently
worker.user('\''www-data'\'', '\''www-data'\'') if Process.euid == 0
end
#unicorn.rb Ends here' > unicorn.rb
chown www-data:www-data unicorn.rb
chown -R www-data:www-data /opt/redmine-2.2.0/tmp
mkdir /etc/unicorn
# Set some config for Unicorn in /etc/unicorn/redmine
echo 'RAILS_ROOT=/opt/redmine-2
RAILS_ENV=production' > /etc/unicorn/redmine
# Create a Unicorn Redmine start script in /etc/init.d/redmine
echo '#! /bin/sh
### BEGIN INIT INFO
# Provides:          redmine
# Required-Start:    $remote_fs $syslog
# Required-Stop:     $remote_fs $syslog
# Default-Start:     2 3 4 5
# Default-Stop:      0 1 6
# Short-Description: redmine initscript
# Description:       This script startup unicorn server and redmine and should
#                    be placed in /etc/init.d.
### END INIT INFO

# ------------------------------------------------------------------------------
# Author: Richard V?zina <ml.richard.vezina@gmail.com>
# Base on Ubuntu 12.04 : /etc/init.d/skeleton
# ven 21 d?c 2012 11:08:31 EST
# ------------------------------------------------------------------------------

# Do NOT "set -e"

# PATH should only include /usr/* if it runs after the mountnfs.sh script
APP=/opt/redmine-2.2.0/
PATH=/sbin:/usr/sbin:/bin:/usr/bin
DESC="Unicorn and Redmine"
NAME=unicorn_rails
DAEMON=/usr/local/bin/$NAME
DAEMON_ARGS=" -E production -c $APP/config/unicorn.rb -D"
PIDFILE=/opt/redmine-2.2.0/tmp/pids/$NAME.pid
SCRIPTNAME=/etc/init.d/redmine

# Exit if the package is not installed
[ -x "$DAEMON" ] || exit 0

# Read configuration variable file if it is present
[ -r /etc/default/$NAME ] && . /etc/default/$NAME

# Load the VERBOSE setting and other rcS variables
. /lib/init/vars.sh

# Define LSB log_* functions.
# Depend on lsb-base (>= 3.2-14) to ensure that this file is present
# and status_of_proc is working.
. /lib/lsb/init-functions

#
# Function that starts the daemon/service
#
do_start()
{
        # Return
        #   0 if daemon has been started
        #   1 if daemon was already running
        #   2 if daemon could not be started
        start-stop-daemon --start --quiet --pidfile $PIDFILE --exec $DAEMON --test > /dev/null \
                || return 1
        start-stop-daemon --start --quiet --pidfile $PIDFILE --exec $DAEMON -- \
                $DAEMON_ARGS \
                || return 2
        # Add code here, if necessary, that waits for the process to be ready
        # to handle requests from services started subsequently which depend
        # on this one.  As a last resort, sleep for some time.
}

#
# Function that stops the daemon/service
#
do_stop()
{
        # Return
        #   0 if daemon has been stopped
        #   1 if daemon was already stopped
        #   2 if daemon could not be stopped
        #   other if a failure occurred
        start-stop-daemon --stop --quiet --retry=TERM/30/KILL/5 --pidfile $PIDFILE --name $NAME
        RETVAL="$?"
        [ "$RETVAL" = 2 ] && return 2
        # Wait for children to finish too if this is a daemon that forks
        # and if the daemon is only ever run from this initscript.
        # If the above conditions are not satisfied then add some other code
        # that waits for the process to drop all resources that could be
        # needed by services started subsequently.  A last resort is to
        # sleep for some time.
        start-stop-daemon --stop --quiet --oknodo --retry=0/30/KILL/5 --exec $DAEMON
        [ "$?" = 2 ] && return 2
        # Many daemons don'\''t delete their pidfiles when they exit.
        rm -f $PIDFILE
        return "$RETVAL"
}

#
# Function that sends a SIGHUP to the daemon/service
#
do_reload() {
        #
        # If the daemon can reload its configuration without
        # restarting (for example, when it is sent a SIGHUP),
        # then implement that here.
        #
        start-stop-daemon --stop --signal 1 --quiet --pidfile $PIDFILE --name $NAME
        return 0
}

case "$1" in
  start)
        [ "$VERBOSE" != no ] && log_daemon_msg "Starting $DESC" "$NAME"
        do_start
        case "$?" in
                0|1) [ "$VERBOSE" != no ] && log_end_msg 0 ;;
                2) [ "$VERBOSE" != no ] && log_end_msg 1 ;;
        esac
        ;;
  stop)
        [ "$VERBOSE" != no ] && log_daemon_msg "Stopping $DESC" "$NAME"
        do_stop
        case "$?" in
                0|1) [ "$VERBOSE" != no ] && log_end_msg 0 ;;
                2) [ "$VERBOSE" != no ] && log_end_msg 1 ;;
        esac
        ;;
  status)
       status_of_proc "$DAEMON" "$NAME" && exit 0 || exit $?
       ;;
  #reload|force-reload)
        #
        # If do_reload() is not implemented then leave this commented out
        # and leave '\''force-reload'\'' as an alias for '\''restart'\''.
        #
        #log_daemon_msg "Reloading $DESC" "$NAME"
        #do_reload
        #log_end_msg $?
        #;;
  restart|force-reload)
        #
        # If the "reload" option is implemented then remove the
        # '\''force-reload'\'' alias
        #
        log_daemon_msg "Restarting $DESC" "$NAME"
        do_stop
        case "$?" in
          0|1)
                do_start
                case "$?" in
                        0) log_end_msg 0 ;;
                        1) log_end_msg 1 ;; # Old process is still running
                        *) log_end_msg 1 ;; # Failed to start
                esac
                ;;
          *)
                  # Failed to stop
                log_end_msg 1
                ;;
        esac
        ;;
  *)
        #echo "Usage: $SCRIPTNAME {start|stop|restart|reload|force-reload}" >&2
        echo "Usage: $SCRIPTNAME {start|stop|status|restart|force-reload}" >&2
        exit 3
        ;;
esac

:' > /etc/init.d/redmine
chmod +x /etc/init.d/redmine
# Backup default Nginx site and replace it
cp /etc/nginx/sites-available/default /etc/nginx/sites-available/default_original
rm /etc/nginx/sites-available/default
# Create configuration file /etc/nginx/sites-available/default
echo 'upstream unicorn_server {
   # This is the socket we configured in unicorn.rb
   server unix:/tmp/unicorn_rails.socket
   fail_timeout=0;
}
server {
        listen          80;
        #return 301 https://192.168.1.126$request_uri; # http://$hostname$request_uri; # idem #http://wiki.nginx.org/Pitfalls#Taxing_Rewrites
        charset utf-8;
        server_name     localhost; # $hostname;
        root /var/www;
        access_log  /var/log/nginx/yoursite.access.log;
        error_log  /var/log/nginx/yoursite.error.log;
        #to enable correct use of response.static_version
        location ~* /(\w+)/static(?:/_[\d]+\.[\d]+\.[\d]+)?/(.*)$ {
            alias /home/www-data/web2py/applications/$1/static/$2;
            expires max;
        }
        location ~^\/(?!redmine(.*)) {
            #uwsgi_pass      127.0.0.1:9001;
            uwsgi_pass      unix:///tmp/web2py.socket;
            include         uwsgi_params;
            uwsgi_param     UWSGI_SCHEME $scheme;
            uwsgi_param     SERVER_SOFTWARE    nginx/$nginx_version;
        }
        location / {
            proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
            proxy_set_header Host $http_host;
            proxy_redirect off;

            if (!-f $request_filename) {
                proxy_pass http://unicorn_server;
                break;
            }
        }
}
server {
        listen 443 default_server ssl;
        charset utf-8;
        server_name     localhost; # $hostname;
        root /var/www;
        ssl_certificate         /etc/nginx/ssl/self_signed.cert;
        ssl_certificate_key     /etc/nginx/ssl/self_signed.key;
                ssl_prefer_server_ciphers on;
                ssl_session_cache shared:SSL:10m;
                ssl_session_timeout 10m;
                ssl_ciphers ECDHE-RSA-AES256-SHA:DHE-RSA-AES256-SHA:DHE-DSS-AES256-SHA:DHE-RSA-AES128-SHA:DHE-DSS-AES128-SHA;
                ssl_protocols SSLv3 TLSv1;
                keepalive_timeout    70;
        location ~* /(\w+)/static(?:/_[\d]+\.[\d]+\.[\d]+)?/(.*)$ {
            alias /home/www-data/web2py/applications/$1/static/$2;
            expires max;
        }
        location ~^\/(?!redmine(.*)) {
            #uwsgi_pass      127.0.0.1:9001;
            uwsgi_pass      unix:///tmp/web2py.socket;
            include         uwsgi_params;
            uwsgi_param     UWSGI_SCHEME $scheme;
            uwsgi_param     SERVER_SOFTWARE    nginx/$nginx_version;
        }
        location / {
            proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
            proxy_set_header Host $http_host;
            proxy_redirect off;

            if (!-f $request_filename) {
                proxy_pass http://unicorn_server;
                break;
            }
        }

}' >/etc/nginx/sites-available/default

#ln -s /etc/nginx/sites-available/web2py /etc/nginx/sites-enabled/web2py
#rm /etc/nginx/sites-enabled/default

# We copy ssl files we previously created
if [ -f /etc/nginx/ssl ]
then
        cp ~/self_signed.* /etc/nginx/ssl/
        rm ~/self_signed.*
else
    mkdir /etc/nginx/ssl
    cp ~/self_signed.* /etc/nginx/ssl/
    rm ~/self_signed.*
fi

pip install setuptools --no-use-wheel --upgrade
PIPPATH=`which pip`
$PIPPATH install --upgrade uwsgi

# Prepare folders for uwsgi
sudo mkdir /etc/uwsgi
sudo mkdir /var/log/uwsgi

# Create configuration file /etc/uwsgi/web2py.xml
echo '<uwsgi>
    <socket>/tmp/web2py.socket</socket>
    <pythonpath>/home/www-data/web2py/</pythonpath>
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
    <uid>www-data</uid>
    <gid>www-data</gid>
    <cron>0 0 -1 -1 -1 python /home/www-data/web2py/web2py.py -Q -S welcome -M -R scripts/sessions2trash.py -A -o</cron>
    <no-orphans/>
</uwsgi>' > /etc/uwsgi/web2py.xml

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
rm web2py_src.zip
chown -R www-data:www-data web2py
cd /home/www-data/web2py
mv handlers/wsgihandler.py wsgihandler.py
sudo -u www-data python -c "from gluon.main import save_password; save_password('$PW',443)"
/etc/init.d/redmine start
start uwsgi-emperor
/etc/init.d/nginx restart
ufw allow 80 # Or check your firewall configuration
