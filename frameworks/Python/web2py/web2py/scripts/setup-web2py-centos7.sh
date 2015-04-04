echo "This script will:
1) Install modules needed to run web2py on Fedora and CentOS/RHEL
2) Install Python 2.6 to /opt and recompile wsgi if not provided
2) Install web2py in /opt/web-apps/
3) Configure SELinux and iptables
5) Create a self signed ssl certificate
6) Setup web2py with mod_wsgi
7) Create virtualhost entries so that web2py responds for '/'
8) Restart Apache.

You should probably read this script before running it.

Although SELinux permissions changes have been made,
further SELinux changes will be required for your personal
apps. (There may also be additional changes required for the
bundled apps.)  As a last resort, SELinux can be disabled.

A simple iptables configuration has been applied.  You may
want to review it to verify that it meets your needs.

Finally, if you require a proxy to access the Internet, please
set up your machine to do so before running this script.

(author: berubejd)

Press ENTER to continue...[ctrl+C to abort]"

read CONFIRM

#!/bin/bash

# (modified for centos7: Dragan (spamperakojotgenije@gmail.com)

###
###  Phase 0 - This may get messy.  Lets work from a temporary directory
###

current_dir=`pwd`

if [ -d /tmp/setup-web2py/ ]; then
    mv /tmp/setup-web2py/ /tmp/setup-web2py.old/
fi

mkdir -p /tmp/setup-web2py
cd /tmp/setup-web2py

###
###  Phase 1 - Requirements installation
###

echo
echo " - Installing packages"
echo

# Verify packages are up to date
yum update

# Install required packages
yum install httpd mod_ssl mod_wsgi wget python unzip

###
### Phase 2 - Install web2py
###

echo
echo " - Downloading, installing, and starting web2py"
echo

# Create web-apps directory, if required
if [ ! -d "/opt/web-apps" ]; then
    mkdir -p /opt/web-apps

    chmod 755 /opt
    chmod 755 /opt/web-apps
fi

cd /opt/web-apps

# Download web2py
if [ -e web2py_src.zip* ]; then
    rm web2py_src.zip*
fi

wget http://web2py.com/examples/static/web2py_src.zip
unzip web2py_src.zip
mv web2py/handlers/wsgihandler.py web2py/wsgihandler.py
chown -R apache:apache web2py

###
### Phase 3 - Setup SELinux context
###
### SELinux doesn't behave well with web2py, for details
### see https://groups.google.com/forum/?fromgroups#!searchin/web2py/selinux/web2py/_thPGA9YhK4/dSnvF3D_lswJ
###
### For now you'll have to disable SELinux


# Allow http_tmp_exec required for wsgi
RETV=`setsebool -P httpd_tmp_exec on > /dev/null 2>&1; echo $?`
if [ ! ${RETV} -eq 0 ]; then
    # CentOS doesn't support httpd_tmp_exec
    cd /tmp/setup-web2py

    # Create the SELinux policy
cat > httpd.te <<EOF

module httpd 1.0;

require {
    type httpd_t;
    class process execmem;
}

#============= httpd_t ==============
allow httpd_t self:process execmem;
EOF

    checkmodule -M -m -o httpd.mod httpd.te
    semodule_package -o httpd.pp -m httpd.mod
    semodule -i httpd.pp

fi

# Setup the overall web2py SELinux context
cd /opt
chcon -R -t httpd_user_content_t web-apps/

cd /opt/web-apps/web2py/applications

# Setup the proper context on the writable application directories
for app in `ls`
do
    for dir in databases cache errors sessions private uploads
    do
        mkdir ${app}/${dir}
        chown apache:apache ${app}/${dir}
        chcon -R -t tmp_t ${app}/${dir}
    done
done


###
### Phase 4 - Configure iptables
###

cd /tmp/setup-web2py

# Create rules file - based upon
# http://articles.slicehost.com/assets/2007/9/4/iptables.txt

# centos7 uses firewalld

firewall-cmd --zone=public --add-port=80/tcp --permanent
firewall-cmd --zone=public --add-port=443/tcp --permanent
firewall-cmd --zone=public --add-port=22/tcp --permanent

firewall-cmd --reload

###
### Phase 5 - Setup SSL
###

echo
echo " - Creating a self signed certificate"
echo

# Verify ssl directory exists
if [ ! -d "/etc/httpd/ssl" ]; then
    mkdir -p /etc/httpd/ssl
fi

# Generate and protect certificate
openssl genrsa 1024 > /etc/httpd/ssl/self_signed.key
openssl req -new -x509 -nodes -sha1 -days 365 -key /etc/httpd/ssl/self_signed.key > /etc/httpd/ssl/self_signed.cert
openssl x509 -noout -fingerprint -text < /etc/httpd/ssl/self_signed.cert > /etc/httpd/ssl/self_signed.info

chmod 400 /etc/httpd/ssl/self_signed.*

###
### Phase 6 - Configure Apache
###

echo
echo " - Configure Apache to use mod_wsgi"
echo

# Create config
if [ -e /etc/httpd/conf.d/welcome.conf ]; then
    mv /etc/httpd/conf.d/welcome.conf /etc/httpd/conf.d/welcome.conf.disabled
fi

cat  > /etc/httpd/conf.d/default.conf <<EOF

NameVirtualHost *:80
NameVirtualHost *:443

<VirtualHost *:80>
  WSGIDaemonProcess web2py user=apache group=apache processes=1 threads=1
  WSGIProcessGroup web2py
  WSGIScriptAlias / /opt/web-apps/web2py/wsgihandler.py
  WSGIPassAuthorization On

  <Directory /opt/web-apps/web2py>
    AllowOverride None
    Order Allow,Deny
    Deny from all    
    <Files wsgihandler.py>
      Require all granted
      Allow from all
    </Files>
  </Directory>

  AliasMatch ^/([^/]+)/static/(?:_[\d]+.[\d]+.[\d]+/)?(.*) /opt/web-apps/web2py/applications/\$1/static/\$2

  <Directory /opt/web-apps/web2py/applications/*/static>
    Options -Indexes
    Order Allow,Deny
    Allow from all
    Require all granted
  </Directory>

  <Location /admin>
    Deny from all
  </Location>

  <LocationMatch ^/([^/]+)/appadmin>
    Deny from all
  </LocationMatch>

  CustomLog /var/log/httpd/access_log common
  ErrorLog /var/log/httpd/error_log
</VirtualHost>

<VirtualHost *:443>
  SSLEngine on
  SSLCertificateFile /etc/httpd/ssl/self_signed.cert
  SSLCertificateKeyFile /etc/httpd/ssl/self_signed.key

  WSGIProcessGroup web2py
  WSGIScriptAlias / /opt/web-apps/web2py/wsgihandler.py
  WSGIPassAuthorization On

  <Directory /opt/web-apps/web2py>
    AllowOverride None
    Order Allow,Deny
    Deny from all
    <Files wsgihandler.py>
      Require all granted
      Allow from all
    </Files>
  </Directory>

  AliasMatch ^/([^/]+)/static/(?:_[\d]+.[\d]+.[\d]+/)?(.*) /opt/web-apps/web2py/applications/\$1/static/\$2

  <Directory /opt/web-apps/web2py/applications/*/static>
    Options -Indexes
    ExpiresActive On
    ExpiresDefault "access plus 1 hour"
    Order Allow,Deny
    Allow from all
    Require all granted
  </Directory>

  CustomLog /var/log/httpd/access_log common
  ErrorLog /var/log/httpd/error_log
</VirtualHost>

EOF

# Fix wsgi socket locations
echo "WSGISocketPrefix run/wsgi" >> /etc/httpd/conf.d/wsgi.conf

# Restart Apache to pick up changes
systemctl restart httpd.service

###
### Phase 7 - Setup web2py admin password
###

echo
echo " - Setup web2py admin password"
echo

cd /opt/web-apps/web2py
sudo -u apache python -c "from gluon.main import save_password; save_password(raw_input('admin password: '),443)"

###
### Phase 8 - Verify that required services start at boot
###

/sbin/chkconfig iptables on
/sbin/chkconfig httpd on

###
### Phase 999 - Done!
###

# Change back to original directory
cd ${current_directory}

echo " - Complete!"
echo
