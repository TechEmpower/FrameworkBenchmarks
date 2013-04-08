import subprocess
import os

class Installer:
  
  ############################################################
  # install_software
  ############################################################
  def install_software(self):
    self.__install_server_software()
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
    self.__run_command("sudo apt-get install build-essential libpcre3 libpcre3-dev libpcrecpp0 libssl-dev zlib1g-dev python-software-properties unzip git-core libcurl4-openssl-dev libbz2-dev libmysqlclient-dev mongodb-clients libreadline6-dev libyaml-dev libsqlite3-dev sqlite3 libxml2-dev libxslt-dev libgdbm-dev ncurses-dev automake libffi-dev htop libtool bison libevent-dev libgstreamer-plugins-base0.10-0 libgstreamer0.10-0 liborc-0.4-0 libwxbase2.8-0 libwxgtk2.8-0 libgnutls-dev libjson0-dev", True)

    self.__run_command("cp ../config/benchmark_profile ../../.bash_profile")

    #######################################
    # Languages
    #######################################

    #
    # Erlang
    #
    self.__run_command("curl -klO https://elearning.erlang-solutions.com/couchdb//rbingen_adapter//package_R16B_precise64_1361901944/esl-erlang_16.b-1~ubuntu~precise_amd64.deb")
    self.__run_command("sudo /usr/bin/dpkg --install esl-erlang_16.b-1~ubuntu~precise_amd64.deb")

    #
    # Python
    #

    self.__run_command("curl http://www.python.org/ftp/python/2.7.3/Python-2.7.3.tgz | tar xvz")
    self.__run_command("./configure", cwd="Python-2.7.3")
    self.__run_command("sudo make install", cwd="Python-2.7.3")
    self.__run_command("curl http://pypi.python.org/packages/source/s/setuptools/setuptools-0.6c11.tar.gz | tar xvz")
    self.__run_command("sudo python setup.py install", cwd="setuptools-0.6c11")
    self.__run_command("curl http://pypi.python.org/packages/source/p/pip/pip-1.1.tar.gz | tar xvz")
    self.__run_command("sudo python setup.py install", cwd="pip-1.1")
    self.__run_command("sudo pip install MySQL-python==1.2.4")
    self.__run_command("sudo pip install simplejson==3.0.7")

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

    self.__run_command("curl http://go.googlecode.com/files/go1.1beta1.linux-amd64.tar.gz | tar xvz")

    #
    # php
    #

    self.__run_command("wget --trust-server-names http://www.php.net/get/php-5.4.13.tar.gz/from/us1.php.net/mirror")
    self.__run_command("tar xvf php-5.4.13.tar.gz")
    self.__run_command("./configure --with-pdo-mysql --enable-fpm --with-fpm-user=www-data --with-fpm-group=www-data", cwd="php-5.4.13")
    self.__run_command("make", cwd="php-5.4.13")
    self.__run_command("sudo make install", cwd="php-5.4.13")
    self.__run_command("printf \"\\n\" | sudo pecl install apc-beta", cwd="php-5.4.13")
    self.__run_command("sudo cp ../config/php.ini /usr/local/lib/php.ini")
    self.__run_command("sudo cp ../config/php-fpm.conf /usr/local/lib/php-fpm.conf")
    self.__run_command("rm php-5.4.13.tar.gz")

    #
    # Haskell
    #

    self.__run_command("sudo apt-get install ghc cabal-install", True)

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
    self.__run_command("curl http://nginx.org/download/nginx-1.2.7.tar.gz | tar xvz")
    self.__run_command("./configure", cwd="nginx-1.2.7")
    self.__run_command("make", cwd="nginx-1.2.7")
    self.__run_command("sudo make install", cwd="nginx-1.2.7")
    
    #
    # Gunicorn
    #

    self.__run_command("sudo easy_install -U 'gunicorn==0.17.2'")
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
    self.__run_command("sudo pip install flask flask-sqlalchemy")

    ##############################
    # Play
    ##############################
    self.__run_command("wget http://downloads.typesafe.com/play/2.1.1/play-2.1.1.zip")
    self.__run_command("unzip -o play-2.1.1.zip")
    self.__run_command("rm play-2.1.1.zip")

    ##############################
    # Vert.x
    ##############################
    self.__run_command("curl http://vertx.io/downloads/vert.x-1.3.1.final.tar.gz | tar xvz")

    ##############################
    # WebGO
    ##############################
    self.__run_command("go/bin/go get github.com/hoisie/web")

    ##############################
    # Yesod
    ##############################
    self.__run_command("cabal update")
    self.__run_command("cabal install yesod persistent-mysql")

    ##############################
    # Snap
    ##############################
    self.__run_command("cabal update")
    self.__run_command("cabal install HDBC HDBC-mysql MonadCatchIO-transformers configurator json snap-core snap-server resource-pool")

    ##############################################################
    #
    # System Tools
    #
    ##############################################################

    ##############################
    # Maven
    ##############################
    self.__run_command("sudo apt-get install maven2", send_yes=True)

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
    yes | sudo apt-get install build-essential git libev-dev libpq-dev libreadline6-dev

    ##############################
    # MySQL
    ##############################
    sudo sh -c "echo mysql-server mysql-server/root_password_again select secret | debconf-set-selections"
    sudo sh -c "echo mysql-server mysql-server/root_password select secret | debconf-set-selections"

    yes | sudo apt-get install mysql-server

    # use the my.cnf file to overwrite /etc/mysql/my.cnf
    sudo mv /etc/mysql/my.cnf /etc/mysql/my.cnf.orig
    sudo mv my.cnf /etc/mysql/my.cnf
    sudo restart mysql

    # Insert data
    mysql -uroot -psecret < create.sql

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
    sudo apt-key adv --keyserver keyserver.ubuntu.com --recv 7F0CEB10
    sudo cp config/10gen.list /etc/apt/sources.list.d/10gen.list
    sudo apt-get update 
    yes | sudo apt-get install mongodb-10gen
    
    sudo mv /etc/mongodb.conf /etc/mongodb.conf.orig
    sudo mv mongodb.conf /etc/mongodb.conf
    sudo restart mongodb
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
