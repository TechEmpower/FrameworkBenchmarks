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
yum install httpd mod_ssl mod_wsgi wget python

# Verify we have at least Python 2.5
typeset -i version_major
typeset -i version_minor

version=`rpm --qf %{Version} -q python`
version_major=`echo ${version} | awk '{split($0, parts, "."); print parts[1]}'`
version_minor=`echo ${version} | awk '{split($0, parts, "."); print parts[2]}'`

if [ ! ${version_major} -ge 2 -o ! ${version_minor} -ge 5 ]; then
    # Setup 2.6 in /opt - based upon
    # http://markkoberlein.com/getting-python-26-with-django-11-together-on

    # Check for earlier Python 2.6 install
    if [ -e /opt/python2.6 ]; then
        # Is Python already installed?
        RETV=`/opt/python2.6/bin/python -V > /dev/null 2>&1; echo $?`
        if [ ${RETV} -eq 0 ]; then
            python_installed='True'
        else
            mv /opt/python2.6 /opt/python2.6-old
        fi
    fi

    # Install Python 2.6 if it doesn't exist already
    if [ ! "${python_installed}" == "True" ]; then
        # Install requirements for the Python build
        yum install sqlite-devel zlib-devel

        mkdir -p /opt/python2.6

        # Download and install
        wget http://www.python.org/ftp/python/2.6.4/Python-2.6.4.tgz
        tar -xzf Python-2.6.4.tgz
        cd Python-2.6.4
        ./configure --prefix=/opt/python2.6 --with-threads --enable-shared --with-zlib=/usr/include
        make && make install

        cd /tmp/setup-web2py
    fi

    # Create links for Python 2.6
    # even if it was previously installed just to be sure
    ln -s /opt/python2.6/lib/libpython2.6.so /usr/lib
    ln -s /opt/python2.6/lib/libpython2.6.so.1.0 /usr/lib
    ln -s /opt/python2.6/bin/python /usr/local/bin/python
    ln -s /opt/python2.6/bin/python /usr/bin/python2.6
    ln -s /opt/python2.6/lib/python2.6.so /opt/python2.6/lib/python2.6/config/

    # Update linker for new libraries
    /sbin/ldconfig

    # Rebuild wsgi to take advantage of Python 2.6
    yum install httpd-devel

    cd /tmp/setup-web2py

    wget http://modwsgi.googlecode.com/files/mod_wsgi-3.3.tar.gz
    tar -xzf mod_wsgi-3.3.tar.gz
    cd mod_wsgi-3.3
    ./configure --with-python=/usr/local/bin/python
    make &&  make install

    echo "LoadModule wsgi_module modules/mod_wsgi.so" > /etc/httpd/conf.d/wsgi.conf

    cd /tmp/setup-web2py
fi

### MySQL install untested!
# Install mysql packages (optional)
#yum install mysql mysql-server

# Enable mysql to start at boot (optional)
#chkconfig --levels 235 mysqld on
#service mysqld start

# Configure mysql security settings (not really optional if mysql installed)
#/usr/bin/mysql_secure_installation

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

# Set context for Python libraries if Python 2.6 installed
if [ -d /opt/python2.6 ]; then
    cd /opt/python2.6
    chcon -R -t lib_t lib/
fi

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
cat > iptables.rules <<EOF
*filter

#  Allows all loopback (lo0) traffic
#  drop all traffic to 127/8 that doesn't use lo0
-A INPUT -i lo -j ACCEPT
-A INPUT ! -i lo -d 127.0.0.0/8 -j REJECT

#  Accepts all established inbound connections
-A INPUT -m state --state ESTABLISHED,RELATED -j ACCEPT

#  Allows all outbound traffic
-A OUTPUT -j ACCEPT

# Allows SSH, HTTP, and HTTPS
# Consider changing the SSH port and/or using rate limiting
# see http://blog.andrew.net.au/2005/02/16#ipt_recent_and_ssh_attacks
-A INPUT -p tcp -m state --state NEW --dport 22 -j ACCEPT
-A INPUT -p tcp --dport 80 -j ACCEPT
-A INPUT -p tcp --dport 443 -j ACCEPT

# Allow ping
-A INPUT -p icmp -m icmp --icmp-type 8 -j ACCEPT

# log iptables denied calls
-A INPUT -m limit --limit 5/min -j LOG --log-prefix "iptables denied: " --log-level 7

# Reject all other inbound - default deny unless explicitly allowed policy
-A INPUT -j REJECT
-A FORWARD -j REJECT

COMMIT
EOF

/sbin/iptables -F
cat iptables.rules | /sbin/iptables-restore
/sbin/service iptables save

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
      Allow from all
    </Files>
  </Directory>

  AliasMatch ^/([^/]+)/static/(?:_[\d]+.[\d]+.[\d]+/)?(.*) /opt/web-apps/web2py/applications/\$1/static/\$2

  <Directory /opt/web-apps/web2py/applications/*/static>
    Options -Indexes
    Order Allow,Deny
    Allow from all
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
  </Directory>

  CustomLog /var/log/httpd/access_log common
  ErrorLog /var/log/httpd/error_log
</VirtualHost>

EOF

# Fix wsgi socket locations
echo "WSGISocketPrefix run/wsgi" >> /etc/httpd/conf.d/wsgi.conf

# Restart Apache to pick up changes
service httpd restart

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
