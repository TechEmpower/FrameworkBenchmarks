import subprocess
import os

class Installer:

  ############################################################
  # install_software
  ############################################################
  def install_software(self):
    if self.benchmarker.install == 'all' or self.benchmarker.install == 'server':
        self.__install_server_software()

    if self.benchmarker.install == 'all' or self.benchmarker.install == 'client':
        self.__install_client_software()
  ############################################################
  # End install_software
  ############################################################

  ############################################################
  # __install_server_software
  ############################################################
  def __install_server_software(self):
    #######################################
    # Prerequisites
    #######################################
    self.__run_command("sudo apt-get update", True)
    self.__run_command("sudo apt-get upgrade", True)
    self.__run_command("sudo apt-get install build-essential libpcre3 libpcre3-dev libpcrecpp0 libssl-dev zlib1g-dev python-software-properties unzip git-core libcurl4-openssl-dev libbz2-dev libmysqlclient-dev mongodb-clients libreadline6-dev libyaml-dev libsqlite3-dev sqlite3 libxml2-dev libxslt-dev libgdbm-dev ncurses-dev automake libffi-dev htop libtool bison libevent-dev libgstreamer-plugins-base0.10-0 libgstreamer0.10-0 liborc-0.4-0 libwxbase2.8-0 libwxgtk2.8-0 libgnutls-dev libjson0-dev libmcrypt-dev libicu-dev cmake gettext", True)
    self.__run_command("sudo add-apt-repository ppa:ubuntu-toolchain-r/test", True)
    self.__run_command("sudo apt-get update", True)
    self.__run_command("sudo apt-get install gcc-4.8 g++-4.8", True)
    
    self.__run_command("cp ../config/benchmark_profile ../../.bash_profile")
    self.__run_command("sudo sh -c \"echo '*               soft    nofile          8192' >> /etc/security/limits.conf\"")

    #######################################
    # Languages
    #######################################

    #
    # Erlang
    #
    self.__run_command("sudo cp ../config/erlang.list /etc/apt/sources.list.d/erlang.list")
    self.__run_command("wget -O - http://binaries.erlang-solutions.com/debian/erlang_solutions.asc | sudo apt-key add -")
    self.__run_command("sudo apt-get update")
    self.__run_command("sudo apt-get install esl-erlang", True)

    #
    # Python
    #

    self.__run_command("curl -L http://bitbucket.org/pypy/pypy/downloads/pypy-2.0-linux64.tar.bz2 | tar xvj")
    self.__run_command("curl http://www.python.org/ftp/python/2.7.4/Python-2.7.4.tgz | tar xvz")
    self.__run_command("./configure", cwd="Python-2.7.4")
    self.__run_command("make -j", cwd="Python-2.7.4")
    self.__run_command("sudo make install", cwd="Python-2.7.4")
    self.__run_command("curl https://pypi.python.org/packages/source/d/distribute/distribute-0.6.38.tar.gz | tar xvz")
    # run pypy before python. (`setup.py install` fails after `sudo setup.py install`)
    self.__run_command("../pypy-2.0/bin/pypy setup.py install", cwd="distribute-0.6.38")
    self.__run_command("sudo python setup.py install", cwd="distribute-0.6.38")
    self.__run_command("curl https://pypi.python.org/packages/source/p/pip/pip-1.3.1.tar.gz | tar xvz")
    self.__run_command("../pypy-2.0/bin/pypy setup.py install", cwd="pip-1.3.1")
    self.__run_command("sudo python setup.py install", cwd="pip-1.3.1")
    self.__run_command("sudo pip install MySQL-python==1.2.4")
    self.__run_command("sudo pip install simplejson==3.0.7")
    self.__run_command("curl http://initd.org/psycopg/tarballs/PSYCOPG-2-5/psycopg2-2.5.tar.gz | tar xvz")
    self.__run_command("sudo python setup.py install", cwd="psycopg2-2.5")
    self.__run_command("git clone https://github.com/iiilx/django-psycopg2-pool.git")
    self.__run_command("sudo python setup.py install", cwd="django-psycopg2-pool")
    self.__run_command("sudo pip install --upgrade numpy==1.7.1")
    self.__run_command("pypy-2.0/bin/pip install PyMySQL==0.5")

    #
    # nodejs
    #

    self.__run_command("curl http://nodejs.org/dist/v0.10.2/node-v0.10.2-linux-x64.tar.gz | tar xvz")

    #
    # Java
    #

    self.__run_command("sudo apt-get install openjdk-7-jdk", True)
    self.__run_command("sudo apt-get remove --purge openjdk-6-jre openjdk-6-jre-headless", True)

    #
    # Ruby/JRuby
    #

    self.__run_command("curl -L get.rvm.io | bash -s head")
    self.__run_command("echo rvm_auto_reload_flag=2 >> ~/.rvmrc")
    subprocess.call(["bash", "-c", "source ~/.rvm/scripts/'rvm' && rvm install 2.0.0-p0"])
    subprocess.call(["bash", "-c", "source ~/.rvm/scripts/'rvm' && rvm 2.0.0-p0 do gem install bundler"])
    subprocess.call(["bash", "-c", "source ~/.rvm/scripts/'rvm' && rvm install jruby-1.7.3"])
    subprocess.call(["bash", "-c", "source ~/.rvm/scripts/'rvm' && rvm jruby-1.7.3 do gem install bundler"])

    # We need a newer version of jruby-rack
    self.__run_command("git clone git://github.com/jruby/jruby-rack.git")
    subprocess.call(["bash", "-c", "cd installs/jruby-rack && source ~/.rvm/scripts/'rvm' && rvm jruby-1.7.3 do bundle install"])
    subprocess.call(["bash", "-c", "cd installs/jruby-rack && source ~/.rvm/scripts/'rvm' && rvm jruby-1.7.3 do jruby -S bundle exec rake clean gem SKIP_SPECS=true"])
    subprocess.call(["bash", "-c", "cd installs/jruby-rack/target && source ~/.rvm/scripts/'rvm' && rvm jruby-1.7.3 do gem install jruby-rack-1.2.0.SNAPSHOT.gem"])

    #
    # go
    #

    self.__run_command("curl http://go.googlecode.com/files/go1.1.linux-amd64.tar.gz | tar xvz")

    #
    # Perl
    #

    self.__run_command("curl -L http://cpanmin.us | perl - --sudo App::cpanminus")
    self.__run_command("cpanm -S DBI DBD::mysql Kelp Dancer Mojolicious Kelp::Module::JSON::XS Dancer::Plugin::Database Starman Plack JSON Web::Simple DBD::Pg")

    #
    # php
    #

    self.__run_command("wget --trust-server-names http://www.php.net/get/php-5.4.13.tar.gz/from/us1.php.net/mirror")
    self.__run_command("tar xvf php-5.4.13.tar.gz")
    self.__run_command("./configure --with-pdo-mysql --with-mysql --with-mcrypt --enable-intl --enable-mbstring --enable-fpm --with-fpm-user=www-data --with-fpm-group=www-data", cwd="php-5.4.13")
    self.__run_command("make", cwd="php-5.4.13")
    self.__run_command("sudo make install", cwd="php-5.4.13")
    self.__run_command("printf \"\\n\" | sudo pecl install apc-beta", cwd="php-5.4.13")
    self.__run_command("sudo cp ../config/php.ini /usr/local/lib/php.ini")
    self.__run_command("sudo cp ../config/php-fpm.conf /usr/local/lib/php-fpm.conf")
    self.__run_command("rm php-5.4.13.tar.gz")

    # Composer
    self.__run_command("curl -sS https://getcomposer.org/installer | php -- --install-dir=bin")

    # Phalcon
    self.__run_command("git clone git://github.com/phalcon/cphalcon.git")
    self.__run_command("sudo ./install", cwd="cphalcon/build")

    #
    # Haskell
    #

    self.__run_command("sudo apt-get install ghc cabal-install", True)

    #
    # RingoJs
    #
    self.__run_command("wget http://www.ringojs.org/downloads/ringojs_0.9-1_all.deb")
    self.__run_command("sudo apt-get install jsvc", True)
    self.__run_command("sudo dpkg -i ringojs_0.9-1_all.deb", True)
    self.__run_command("rm ringojs_0.9-1_all.deb")
    self.__run_command("sudo ringo-admin install oberhamsi/sql-ringojs-client")
    self.__run_command("sudo ringo-admin install ringo/stick")
    self.__run_command("sudo ringo-admin install oberhamsi/reinhardt")
    self.__run_command("sudo ringo-admin install grob/ringo-sqlstore")
    self.__run_command("sudo ringo-admin install amigrave/ringo-mongodb")

    #
    # Mono
    #
    self.__run_command("git clone git://github.com/mono/mono")
    self.__run_command("git checkout mono-3.10", cwd="mono")
    self.__run_command("./autogen.sh --prefix=/usr/local", cwd="mono")
    self.__run_command("make get-monolite-latest", cwd="mono")
    self.__run_command("make EXTERNAL_MCS=${PWD}/mcs/class/lib/monolite/gmcs.exe", cwd="mono")
    self.__run_command("sudo make install", cwd="mono")

    self.__run_command("git clone git://github.com/mono/xsp")
    self.__run_command("git checkout 3.0", cwd="xsp")
    self.__run_command("./autogen.sh --prefix=/usr/local", cwd="xsp")
    self.__run_command("make", cwd="xsp")
    self.__run_command("sudo make install", cwd="xsp")
    #######################################
    # Webservers
    #######################################

    #
    # Apache
    #

    self.__run_command("sudo apt-get install apache2 libapache2-mod-php5", True)
    self.__run_command("sudo mv /etc/apache2/apache2.conf /etc/apache2/apache2.conf.orig")
    self.__run_command("sudo sh -c \"cat ../config/apache2.conf > /etc/apache2/apache2.conf\"")
    self.__run_command("sudo mv /etc/apache2/ports.conf /etc/apache2/ports.conf.orig")
    self.__run_command("sudo sh -c \"cat ../config/ports.conf > /etc/apache2/ports.conf\"")
    self.__run_command("sudo /etc/init.d/apache2 stop")

    #
    # Nginx
    #
    self.__run_command("curl http://nginx.org/download/nginx-1.4.0.tar.gz | tar xvz")
    self.__run_command("./configure", cwd="nginx-1.4.0")
    self.__run_command("make", cwd="nginx-1.4.0")
    self.__run_command("sudo make install", cwd="nginx-1.4.0")

    #
    # Openresty (nginx with openresty stuff)
    #
    self.__run_command("curl http://openresty.org/download/ngx_openresty-1.2.7.5.tar.gz | tar xvz")
    self.__run_command("./configure --with-luajit", cwd="ngx_openresty-1.2.7.5")
    self.__run_command("make", cwd="ngx_openresty-1.2.7.5")
    self.__run_command("sudo make install", cwd="ngx_openresty-1.2.7.5")

    #
    # Gunicorn
    #

    self.__run_command("sudo easy_install -U 'gunicorn==0.17.2'")
    self.__run_command("sudo pip install --upgrade meinheld")
    self.__run_command("sudo easy_install -U 'eventlet==0.12.1'")
    self.__run_command("sudo pip install --upgrade 'gevent==0.13.8'")

    #
    # Resin
    #

    self.__run_command("sudo cp -r /usr/lib/jvm/java-1.7.0-openjdk-amd64/include /usr/lib/jvm/java-1.7.0-openjdk-amd64/jre/bin/")
    self.__run_command("curl http://www.caucho.com/download/resin-4.0.34.tar.gz | tar xvz")
    self.__run_command("./configure --prefix=`pwd`", cwd="resin-4.0.34")
    self.__run_command("make", cwd="resin-4.0.34")
    self.__run_command("make install", cwd="resin-4.0.34")
    self.__run_command("mv conf/resin.properties conf/resin.properties.orig", cwd="resin-4.0.34")
    self.__run_command("cat ../config/resin.properties > resin-4.0.34/conf/resin.properties")

    #
    # Passenger
    #

    self.__run_command("git clone https://github.com/FooBarWidget/passenger.git")
    self.__run_command("git checkout 65d36dbbadd399f65d81f5febadce9b0c6c1a430", cwd="passenger")
    subprocess.call(["bash", "-c", "cd installs/passenger && source ~/.rvm/scripts/'rvm' && rvm 2.0.0-p0 do gem build passenger.gemspec"])
    subprocess.call(["bash", "-c", "cd installs/passenger && source ~/.rvm/scripts/'rvm' && rvm 2.0.0-p0 do gem install passenger-3.9.5.rc3.gem"])

    ##############################
    # Tomcat
    # We don't use tomcat in our tests yet, but this is here to remind us of how we
    # installed the apr connector
    ##############################
    self.__run_command("curl http://apache.cs.utah.edu/tomcat/tomcat-7/v7.0.35/bin/apache-tomcat-7.0.35.tar.gz | tar xvz")
    #wget http://apache.cs.utah.edu/tomcat/tomcat-7/v7.0.35/bin/apache-tomcat-7.0.35.tar.gz
    #tar -xvzf apache-tomcat-7.0.35.tar.gz
    #rm apache-tomcat-7.0.35.tar.gz
    # use the native APR
    # http://evgeny-goldin.com/blog/ubuntu-installing-apr-tomcat/
    #wget http://apache.claz.org/apr/apr-1.4.6.tar.gz
    #tar xvf apr-1.4.6.tar.gz
    #cd apr-1.4.6
    #./configure
    #make
    #sudo make install
    #cd ..
    #rm apr-1.4.6.tar.gz
    #wget http://apache.tradebit.com/pub/tomcat/tomcat-connectors/native/1.1.24/source/tomcat-native-1.1.24-src.tar.gz
    #tar xvf tomcat-native-1.1.24-src.tar.gz
    #cd tomcat-native-1.1.24-src/jni/native
    #./configure --with-apr=/usr/local/apr
    #make
    #sudo make install
    #cd ../../..
    #rm tomcat-native-1.1.24-src.tar.gz

    ##############################################################
    #
    # Frameworks
    #
    ##############################################################

    ##############################
    # Tornado
    ##############################
    packages = "tornado==3.0.1 motor==0.1 pymongo==2.5"
    self.__run_command("sudo pip install " + packages)
    self.__run_command("pypy-2.0/bin/pip install " + packages)

    ##############################
    # Django
    ##############################
    self.__run_command("curl http://www.djangoproject.com/m/releases/1.4/Django-1.4.tar.gz | tar xvz")
    self.__run_command("sudo rm -rf /usr/local/lib/python2.7/site-packages/django")
    self.__run_command("sudo python setup.py install", cwd="Django-1.4")
    self.__run_command("sudo easy_install -U 'ujson==1.30'")

    ##############################
    # Grails
    ##############################
    self.__run_command("wget http://dist.springframework.org.s3.amazonaws.com/release/GRAILS/grails-2.1.1.zip")
    self.__run_command("unzip -o grails-2.1.1.zip")
    self.__run_command("rm grails-2.1.1.zip")


    ##############################
    # Flask
    ##############################
    packages = "flask==0.9 flask-sqlalchemy==0.16 sqlalchemy==0.8.1 jinja2==2.6 werkzeug==0.8.3"
    self.__run_command("sudo pip install " + packages)
    self.__run_command("pypy-2.0/bin/pip install " + packages)

    ##############################
    # Bottle
    ##############################
    self.__run_command("sudo pip install bottle bottle-sqlalchemy")

    ##############################
    # Play 2
    ##############################
    self.__run_command("wget http://downloads.typesafe.com/play/2.1.1/play-2.1.1.zip")
    self.__run_command("unzip -o play-2.1.1.zip")
    self.__run_command("rm play-2.1.1.zip")

    ##############################
    # Play 1
    ##############################
    self.__run_command("wget http://downloads.typesafe.com/releases/play-1.2.5.zip")
    self.__run_command("unzip -o play-1.2.5.zip")
    self.__run_command("rm play-1.2.5.zip")
    self.__run_command("mv play-1.2.5/play play-1.2.5/play1")

    # siena
    self.__run_command("play-1.2.5/play1 install siena", send_yes=True)

    ##############################
    # Vert.x
    ##############################
    self.__run_command("curl http://vertx.io/downloads/vert.x-1.3.1.final.tar.gz | tar xvz")

    ##############################
    # Yesod
    ##############################
    self.__run_command("cabal update")
    self.__run_command("cabal install yesod persistent-mysql")

    ##############################################################
    #
    # System Tools
    #
    ##############################################################

    ##############################
    # Maven
    ##############################
    # self.__run_command("sudo apt-get install maven2", send_yes=True)
    self.__run_command("curl www.us.apache.org/dist/maven/maven-3/3.0.5/binaries/apache-maven-3.0.5-bin.tar.gz | tar xvz")

    ##############################
    # Leiningen
    ##############################
    self.__run_command("mkdir -p bin")
    self.__run_command("wget https://raw.github.com/technomancy/leiningen/stable/bin/lein")
    self.__run_command("mv lein bin/lein")
    self.__run_command("chmod +x bin/lein")
  ############################################################
  # End __install_server_software
  ############################################################

  ############################################################
  # __install_client_software
  ############################################################
  def __install_client_software(self):
    subprocess.call(self.benchmarker.sftp_string(batch_file="config/client_sftp_batch"), shell=True)

    remote_script = """

    ##############################
    # Prerequisites
    ##############################
    yes | sudo apt-get update
    yes | sudo apt-get install build-essential git libev-dev libpq-dev libreadline6-dev postgresql
    sudo sh -c "echo '*               soft    nofile          8192' >> /etc/security/limits.conf"

    sudo mkdir -p /ssd
    sudo mkdir -p /ssd/log

    ##############################
    # MySQL
    ##############################
    sudo sh -c "echo mysql-server mysql-server/root_password_again select secret | debconf-set-selections"
    sudo sh -c "echo mysql-server mysql-server/root_password select secret | debconf-set-selections"

    yes | sudo apt-get install mysql-server

    sudo stop mysql
    # use the my.cnf file to overwrite /etc/mysql/my.cnf
    sudo mv /etc/mysql/my.cnf /etc/mysql/my.cnf.orig
    sudo mv my.cnf /etc/mysql/my.cnf

    sudo cp -R -p /var/lib/mysql /ssd/
    sudo cp -R -p /var/log/mysql /ssd/log
    sudo start mysql

    # Insert data
    mysql -uroot -psecret < create.sql

    ##############################
    # Postgres
    ##############################
    sudo useradd benchmarkdbuser -p benchmarkdbpass
    sudo -u postgres psql template1 < create-postgres-database.sql
    sudo -u benchmarkdbuser psql hello_world < create-postgres.sql

    sudo -u postgres -H /etc/init.d/postgresql stop
    sudo mv postgresql.conf /etc/postgresql/9.1/main/postgresql.conf
    sudo mv pg_hba.conf /etc/postgresql/9.1/main/pg_hba.conf

    sudo cp -R -p /var/lib/postgresql/9.1/main /ssd/postgresql
    sudo -u postgres -H /etc/init.d/postgresql start

    ##############################
    # Weighttp
    ##############################

    git clone git://git.lighttpd.net/weighttp
    cd weighttp
    ./waf configure
    ./waf build
    sudo ./waf install
    cd ~

    ##############################
    # wrk
    ##############################

    git clone https://github.com/wg/wrk.git
    cd wrk
    make
    sudo cp wrk /usr/local/bin
    cd ~

    ##############################
    # MongoDB
    ##############################
    sudo apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv 7F0CEB10
    sudo cp 10gen.list /etc/apt/sources.list.d/10gen.list
    sudo apt-get update
    yes | sudo apt-get install mongodb-10gen

    sudo stop mongodb
    sudo mv /etc/mongodb.conf /etc/mongodb.conf.orig
    sudo mv mongodb.conf /etc/mongodb.conf
    sudo cp -R -p /var/lib/mongodb /ssd/
    sudo cp -R -p /var/log/mongodb /ssd/log/
    sudo start mongodb
    """
    p = subprocess.Popen(self.benchmarker.ssh_string.split(" "), stdin=subprocess.PIPE)
    p.communicate(remote_script)

  ############################################################
  # End __parse_results
  ############################################################

  ############################################################
  # __run_command
  ############################################################
  def __run_command(self, command, send_yes=False, cwd=None):
    try:
      cwd = os.path.join(self.install_dir, cwd)
    except AttributeError:
      cwd = self.install_dir

    if send_yes:
      subprocess.Popen(command, shell=True, stdin=subprocess.PIPE, cwd=cwd).communicate("yes")
    else:
      subprocess.call(command, shell=True, cwd=cwd)
  ############################################################
  # End __run_command
  ############################################################

  ############################################################
  # __init__(benchmarker)
  ############################################################
  def __init__(self, benchmarker):
    self.benchmarker = benchmarker
    self.install_dir = "installs"

    try:
      os.mkdir(self.install_dir)
    except OSError:
      pass
  ############################################################
  # End __init__
  ############################################################

