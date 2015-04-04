echo "This script will:
1) install all modules need to run web2py on Ubuntu/Debian
2) install web2py in /home/www-data/
3) create a self signed ssl certificate
4) setup web2py with mod_wsgi
5) overwrite /etc/apache2/sites-available/default
6) restart apache.

You may want to read this script before running it.

Press a key to continue...[ctrl+C to abort]"

read CONFIRM

#!/bin/bash
# optional
# dpkg-reconfigure console-setup
# dpkg-reconfigure timezoneconf
# nano /etc/hostname
# nano /etc/network/interfaces
# nano /etc/resolv.conf
# reboot now
# ifconfig eth0

echo "installing useful packages"
echo "=========================="
apt-get update
apt-get -y install ssh
apt-get -y install zip unzip
apt-get -y install tar
apt-get -y install openssh-server
apt-get -y install build-essential
apt-get -y install ipython
apt-get -y install python-dev
apt-get -y install postgresql
apt-get -y install apache2
apt-get -y install libapache2-mod-wsgi
apt-get -y install python-psycopg2
apt-get -y install postfix
apt-get -y install wget
apt-get -y install python-matplotlib
apt-get -y install python-reportlab
apt-get -y install mercurial
/etc/init.d/postgresql restart

# optional, uncomment for emacs
# apt-get -y install emacs

# optional, uncomment for backups using samba
# apt-get -y install samba
# apt-get -y install smbfs

echo "downloading, installing and starting web2py"
echo "==========================================="
cd /home
mkdir www-data
cd www-data
rm web2py_src.zip*
wget http://web2py.com/examples/static/web2py_src.zip
unzip web2py_src.zip
mv web2py/handlers/wsgihandler.py web2py/wsgihandler.py
chown -R www-data:www-data web2py

echo "setting up apache modules"
echo "========================="
a2enmod ssl
a2enmod proxy
a2enmod proxy_http
a2enmod headers
a2enmod expires
a2enmod wsgi
mkdir /etc/apache2/ssl

echo "creating a self signed certificate"
echo "=================================="
openssl genrsa 1024 > /etc/apache2/ssl/self_signed.key
chmod 400 /etc/apache2/ssl/self_signed.key
openssl req -new -x509 -nodes -sha1 -days 365 -key /etc/apache2/ssl/self_signed.key > /etc/apache2/ssl/self_signed.cert
openssl x509 -noout -fingerprint -text < /etc/apache2/ssl/self_signed.cert > /etc/apache2/ssl/self_signed.info

echo "rewriting your apache config file to use mod_wsgi"
echo "================================================="
echo '
<VirtualHost *:80>
  ServerName YourServerName
  WSGIDaemonProcess web2py user=www-data group=www-data display-name=%{GROUP}
  WSGIProcessGroup web2py
  WSGIScriptAlias / /home/www-data/web2py/wsgihandler.py

  <Directory /home/www-data/web2py>
    Require all denied
    <Files wsgihandler.py>
      Require all granted
    </Files>
  </Directory>

    AliasMatch ^/([^/]+)/static/(?:_[\d]+.[\d]+.[\d]+/)?(.*) \
           /home/www-data/web2py/applications/$1/static/$2
  <Directory /home/www-data/web2py/applications/*/static/>
    Options -Indexes
    Require all granted
  </Directory>

  <Location /admin>
    Require all denied
  </Location>

  <LocationMatch ^/([^/]+)/appadmin>
    Require all denied
  </LocationMatch>

  CustomLog /var/log/apache2/access.log common
  ErrorLog /var/log/apache2/error.log
</VirtualHost>


<VirtualHost *:443>
  ServerName YourServerName
  SSLEngine on
  SSLCertificateFile /etc/apache2/ssl/self_signed.cert
  SSLCertificateKeyFile /etc/apache2/ssl/self_signed.key

  WSGIProcessGroup web2py

  WSGIScriptAlias / /home/www-data/web2py/wsgihandler.py

  <Directory /home/www-data/web2py>
    AllowOverride None
    Require all denied
    <Files wsgihandler.py>
      Require all granted
    </Files>
  </Directory>

   AliasMatch ^/([^/]+)/static/(?:_[\d]+.[\d]+.[\d]+/)?(.*) \
        /home/www-data/web2py/applications/$1/static/$2

  <Directory /home/www-data/web2py/applications/*/static/>
    Require all granted
  </Directory>

  CustomLog /var/log/apache2/access.log common
  ErrorLog /var/log/apache2/error.log

</VirtualHost>
' > /etc/apache2/sites-available/default

# echo "setting up PAM"
# echo "================"
# sudo apt-get install pwauth
# sudo ln -s /etc/apache2/mods-available/authnz_external.load /etc/apache2/mods-enabled
# ln -s /etc/pam.d/apache2 /etc/pam.d/httpd
# usermod -a -G shadow www-data

echo "restarting apache"
echo "================"

/etc/init.d/apache2 restart
cd /home/www-data/web2py
sudo -u www-data python -c "from gluon.widget import console; console();"
sudo -u www-data python -c "from gluon.main import save_password; save_password(raw_input('admin password: '),443)"
echo "done!"
