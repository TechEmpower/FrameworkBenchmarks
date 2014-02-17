import subprocess
import os
import time
import traceback
import sys

class Installer:

  ############################################################
  # install_software
  ############################################################
  def install_software(self):
    if self.benchmarker.install == 'all' or self.benchmarker.install == 'server':
        self.__install_server_software()

    if self.benchmarker.install == 'all' or self.benchmarker.install == 'database':
        self.__install_database_software()

    if self.benchmarker.install == 'all' or self.benchmarker.install == 'client':
        self.__install_client_software()
  ############################################################
  # End install_software
  ############################################################

  ############################################################
  # __install_server_software
  ############################################################
  def __install_server_software(self):
    print("\nINSTALL: Installing server software\n")

    #######################################
    # Prerequisites
    #######################################
    self.__run_command("sudo apt-get update", True)
    self.__run_command("sudo apt-get upgrade", True)
    self.__run_command("sudo apt-get install build-essential libpcre3 libpcre3-dev libpcrecpp0 libssl-dev zlib1g-dev python-software-properties unzip git-core libcurl4-openssl-dev libbz2-dev libmysqlclient-dev mongodb-clients libreadline6-dev libyaml-dev libsqlite3-dev sqlite3 libxml2-dev libxslt-dev libgdbm-dev ncurses-dev automake libffi-dev htop libtool bison libevent-dev libgstreamer-plugins-base0.10-0 libgstreamer0.10-0 liborc-0.4-0 libwxbase2.8-0 libwxgtk2.8-0 libgnutls-dev libjson0-dev libmcrypt-dev libicu-dev cmake gettext curl libpq-dev mercurial mlton", True)
    self.__run_command("sudo add-apt-repository ppa:ubuntu-toolchain-r/test", True)
    self.__run_command("sudo apt-get update", True)
    self.__run_command("sudo apt-get install gcc-4.8 g++-4.8", True)

    self.__run_command("cp ../config/benchmark_profile ../../.bash_profile")
    self.__run_command("cat ../config/benchmark_profile >> ../../.profile")
    self.__run_command("cat ../config/benchmark_profile >> ../../.bashrc")
    self.__run_command("source ../../.profile")
    self.__run_command("sudo sh -c \"echo '*               -    nofile          65535' >> /etc/security/limits.conf\"")

    ##############################################################
    # System Tools
    ##############################################################

    #
    # Leiningen
    #
    self.__run_command("mkdir -p bin")
    self.__download("https://raw.github.com/technomancy/leiningen/stable/bin/lein")
    self.__run_command("mv lein bin/lein")
    self.__run_command("chmod +x bin/lein")

    #
    # Maven
    #
    self.__run_command("sudo apt-get install maven -qq")
    self.__run_command("mvn -version")

    #######################################
    # Languages
    #######################################

    self._install_python()

    #
    # Dart
    #
    self.__download("http://storage.googleapis.com/dart-archive/channels/stable/release/latest/sdk/dartsdk-linux-x64-release.zip")
    self.__run_command("unzip dartsdk-linux-x64-release.zip")

    #
    # Erlang
    #
    self.__run_command("sudo cp ../config/erlang.list /etc/apt/sources.list.d/erlang.list")
    self.__download("http://binaries.erlang-solutions.com/debian/erlang_solutions.asc")
    self.__run_command("sudo apt-key add erlang_solutions.asc")
    self.__run_command("sudo apt-get update")
    self.__run_command("sudo apt-get install esl-erlang", True)

    #
    # nodejs
    #
    self.__download("http://nodejs.org/dist/v0.10.8/node-v0.10.8-linux-x64.tar.gz")
    self.__run_command("tar xzf node-v0.10.8-linux-x64.tar.gz")

    #
    # Java
    #
    self.__run_command("sudo apt-get install openjdk-7-jdk", True)
    self.__run_command("sudo apt-get remove --purge openjdk-6-jre openjdk-6-jre-headless", True)

    #
    # Ruby/JRuby
    #
    self.__run_command("curl -L get.rvm.io | bash -s head --auto-dotfiles")
    self.__run_command("echo rvm_auto_reload_flag=2 >> ~/.rvmrc")
    self.__bash_from_string("source ~/.rvm/scripts/'rvm' && rvm install 2.0.0-p0")
    self.__bash_from_string("source ~/.rvm/scripts/'rvm' && rvm 2.0.0-p0 do gem install bundler")
    self.__bash_from_string("source ~/.rvm/scripts/'rvm' && rvm install jruby-1.7.8")
    self.__bash_from_string("source ~/.rvm/scripts/'rvm' && rvm jruby-1.7.8 do gem install bundler")

    #
    # go
    #
    self.__download("http://go.googlecode.com/files/go1.2.linux-amd64.tar.gz");
    self.__run_command("tar xzf go1.2.linux-amd64.tar.gz")

    #
    # Perl
    #
    self.__download("http://downloads.activestate.com/ActivePerl/releases/5.16.3.1603/ActivePerl-5.16.3.1603-x86_64-linux-glibc-2.3.5-296746.tar.gz");
    self.__run_command("tar xzf ActivePerl-5.16.3.1603-x86_64-linux-glibc-2.3.5-296746.tar.gz");
    self.__run_command("sudo ./install.sh --license-accepted --prefix /opt/ActivePerl-5.16 --no-install-html", cwd="ActivePerl-5.16.3.1603-x86_64-linux-glibc-2.3.5-296746", send_yes=True, retry=True)
    self.__download("http://cpanmin.us", "cpanminus.pl")
    self.__run_command("perl cpanminus.pl --sudo App::cpanminus", retry=True)
    self.__run_command("cpanm -f -S DBI DBD::mysql Kelp Dancer Mojolicious Kelp::Module::JSON::XS Dancer::Plugin::Database Starman Plack JSON Web::Simple DBD::Pg JSON::XS EV HTTP::Parser::XS Monoceros EV IO::Socket::IP IO::Socket::SSL", retry=True)

    #
    # php
    #
    self.__download("http://museum.php.net/php5/php-5.4.13.tar.gz")
    self.__run_command("tar xzf php-5.4.13.tar.gz")
    self.__run_command("./configure --with-pdo-mysql --with-mysql --with-mcrypt --enable-intl --enable-mbstring --enable-fpm --with-fpm-user=www-data --with-fpm-group=www-data --with-openssl", cwd="php-5.4.13")
    self.__run_command("make", cwd="php-5.4.13")
    self.__run_command("sudo make install", cwd="php-5.4.13")
    self.__run_command("printf \"\\n\" | sudo pecl install apc-beta", cwd="php-5.4.13", retry=True)
    self.__run_command("sudo cp ../config/php.ini /usr/local/lib/php.ini")
    self.__run_command("sudo cp ../config/php-fpm.conf /usr/local/lib/php-fpm.conf")

    # Composer
    self.__download("https://getcomposer.org/installer", "composer-installer.php")
    self.__run_command("php composer-installer.php --install-dir=bin")

    # Phalcon
    self.__run_command("git clone git://github.com/phalcon/cphalcon.git", retry=True)
    self.__run_command("sudo ./install", cwd="cphalcon/build")

    # YAF
    self.__run_command("sudo pecl install yaf")

    #
    # Haskell
    #
    self.__run_command("sudo apt-get install ghc cabal-install", True)

    #
    # RingoJs
    #
    self.__download("http://www.ringojs.org/downloads/ringojs_0.9-1_all.deb")
    self.__run_command("sudo apt-get install jsvc", True)
    self.__run_command("sudo dpkg -i ringojs_0.9-1_all.deb", True)
    self.__run_command("rm ringojs_0.9-1_all.deb")

    #
    # Mono - TODO - this install script doesn't work.
    #
    #self.__run_command("git clone git://github.com/mono/mono", retry=True)
    #self.__run_command("git checkout mono-3.2.3", cwd="mono")
    #self.__run_command("./autogen.sh --prefix=/usr/local", cwd="mono")
    #self.__run_command("make get-monolite-latest", cwd="mono")
    #self.__run_command("make EXTERNAL_MCS=${PWD}/mcs/class/lib/monolite/gmcs.exe", cwd="mono")
    #self.__run_command("sudo make install", cwd="mono")

    self.__run_command("mozroots --import --sync", retry=True)

    self.__run_command("git clone git://github.com/mono/xsp", retry=True)
    self.__run_command("./autogen.sh --prefix=/usr/local", cwd="xsp")
    self.__run_command("make", cwd="xsp")
    self.__run_command("sudo make install", cwd="xsp")

    #
    # Nimrod
    #
    self.__run_command("git clone git://github.com/Araq/Nimrod.git nimrod", retry=True)
    self.__run_command("git reset --hard \"43d12ef7495238977e4f236f6d25bce5bd687967\"", cwd="nimrod")
    self.__run_command("git clone --depth 1 git://github.com/nimrod-code/csources.git", cwd="nimrod", retry=True)
    self.__run_command("chmod +x build.sh", cwd="nimrod/csources")
    self.__run_command("./build.sh", cwd="nimrod/csources")
    self.__run_command("bin/nimrod c koch", cwd="nimrod")
    self.__run_command("./koch boot -d:release", cwd="nimrod")

    #
    # Racket
    #
    self.__download("https://github.com/plt/racket/archive/v5.3.6.tar.gz", "racket-5.3.6.tar.gz")
    self.__run_command("tar xzf racket-5.3.6.tar.gz")
    self.__run_command("./configure", cwd="racket/src")
    self.__run_command("make", cwd="racket/src")
    self.__run_command("sudo make install", cwd="racket/src")

    #
    # Ur/Web
    #
    self.__run_command("hg clone http://hg.impredicative.com/urweb")
    self.__run_command("./autogen.sh", cwd="urweb")
    self.__run_command("./configure", cwd="urweb")
    self.__run_command("make", cwd="urweb")
    self.__run_command("sudo make install", cwd="urweb")
    
    #
    # HHVM
    #
    self.__run_command("wget -O - http://dl.hhvm.com/conf/hhvm.gpg.key | sudo apt-key add -")
    self.__run_command("echo deb http://dl.hhvm.com/ubuntu precise main | sudo tee /etc/apt/sources.list.d/hhvm.list")
    self.__run_command("sudo apt-get update")
    self.__run_command("sudo apt-get install hhvm")

    #######################################
    # Webservers
    #######################################

    #
    # Nginx
    #
    self.__download("http://nginx.org/download/nginx-1.4.1.tar.gz")
    self.__run_command("tar xzf nginx-1.4.1.tar.gz")
    self.__run_command("./configure", cwd="nginx-1.4.1")
    self.__run_command("make", cwd="nginx-1.4.1")
    self.__run_command("sudo make install", cwd="nginx-1.4.1")

    #
    # Openresty (nginx with openresty stuff)
    #
    self.__download("http://openresty.org/download/ngx_openresty-1.2.7.5.tar.gz")
    self.__run_command("tar xzf ngx_openresty-1.2.7.5.tar.gz")
    self.__run_command("./configure --with-luajit --with-http_postgres_module", cwd="ngx_openresty-1.2.7.5")
    self.__run_command("make", cwd="ngx_openresty-1.2.7.5")
    self.__run_command("sudo make install", cwd="ngx_openresty-1.2.7.5")

    #
    # Resin
    #

    self.__run_command("sudo cp -r /usr/lib/jvm/java-1.7.0-openjdk-amd64/include /usr/lib/jvm/java-1.7.0-openjdk-amd64/jre/bin/")
    self.__download("http://www.caucho.com/download/resin-4.0.36.tar.gz")
    self.__run_command("tar xzf resin-4.0.36.tar.gz")
    self.__run_command("./configure --prefix=`pwd`", cwd="resin-4.0.36")
    self.__run_command("make", cwd="resin-4.0.36")
    self.__run_command("make install", cwd="resin-4.0.36")
    self.__run_command("mv conf/resin.properties conf/resin.properties.orig", cwd="resin-4.0.36")
    self.__run_command("cat ../config/resin.properties > resin-4.0.36/conf/resin.properties")
    self.__run_command("mv conf/resin.xml conf/resin.xml.orig", cwd="resin-4.0.36")
    self.__run_command("cat ../config/resin.xml > resin-4.0.36/conf/resin.xml")

    #
    # Mongrel2
    #
    self.__download("http://download.zeromq.org/zeromq-4.0.3.tar.gz")
    self.__run_command("tar xzf zeromq-4.0.3.tar.gz")
    self.__run_command("./configure", cwd="zeromq-4.0.3")
    self.__run_command("make", cwd="zeromq-4.0.3")
    self.__run_command("sudo make install", cwd="zeromq-4.0.3")
    self.__run_command("sudo apt-get install sqlite3 libsqlite3-dev uuid uuid-runtime uuid-dev")
    self.__run_command("sudo ldconfig -v")
    self.__run_command("git clone git://github.com/zedshaw/mongrel2.git mongrel2", retry=True)
    # for zmq4, we update the following file manually (not in the stable master branch yet)
    self.__download("https://raw.github.com/zedshaw/mongrel2/9b565eeea003783c47502c2d350b99c9684ce97c/src/zmq_compat.h")
    self.__run_command("mv -f zmq_compat.h mongrel2/src/")
    self.__run_command("make clean all && sudo make install", cwd="mongrel2")

    ##############################################################
    # Frameworks
    ##############################################################

    #
    # Grails
    #
    self.__download("http://dist.springframework.org.s3.amazonaws.com/release/GRAILS/grails-2.3.3.zip")
    self.__run_command("unzip -o grails-2.3.3.zip")

    #
    # Play 2
    #
    self.__download("http://downloads.typesafe.com/play/2.2.0/play-2.2.0.zip")
    self.__run_command("unzip -o play-2.2.0.zip")

    #
    # Play 1
    #
    self.__download("http://downloads.typesafe.com/releases/play-1.2.5.zip")
    self.__run_command("unzip -o play-1.2.5.zip")
    self.__run_command("mv play-1.2.5/play play-1.2.5/play1")

    # siena
    self.__run_command("yes | play-1.2.5/play1 install siena")

    #
    # TreeFrog Framework
    #
    self.__run_command("sudo apt-get install qt4-qmake libqt4-dev libqt4-sql-mysql libqt4-sql-psql g++", True)
    self.__download("http://downloads.sourceforge.net/project/treefrog/src/treefrog-1.7.5.tar.gz")
    self.__run_command("tar xzf treefrog-1.7.5.tar.gz")
    self.__run_command("rm treefrog-1.7.5.tar.gz")
    self.__run_command("./configure", cwd="treefrog-1.7.5")
    self.__run_command("make", cwd="treefrog-1.7.5/src")
    self.__run_command("sudo make install", cwd="treefrog-1.7.5/src")
    self.__run_command("make", cwd="treefrog-1.7.5/tools")
    self.__run_command("sudo make install", cwd="treefrog-1.7.5/tools")

    #
    # Vert.x
    #
    self.__download("http://dl.bintray.com/vertx/downloads/vert.x-2.1M3.tar.gz?direct=true", "vert.x-2.1M3.tar.gz")
    self.__run_command("tar xzf vert.x-2.1M3.tar.gz")

    #
    # Yesod
    #
    self.__run_command("cabal update", retry=True)
    self.__run_command("cabal install yesod persistent-mysql", retry=True)

    #
    # Jester
    #
    self.__run_command("git clone git://github.com/dom96/jester.git jester/jester", retry=True)

    #
    # Onion
    #
    self.__run_command("git clone https://github.com/davidmoreno/onion.git")
    self.__run_command("mkdir build", cwd="onion")
    self.__run_command("cmake ..", cwd="onion/build")
    self.__run_command("make", cwd="onion/build")

    # nawak
    #
    self.__run_command("git clone git://github.com/idlewan/nawak.git nawak/nawak", retry=True)

    print("\nINSTALL: Finished installing server software\n")
  ############################################################
  # End __install_server_software
  ############################################################

  def _install_python(self):
    # .profile is not loaded yet. So we should use full path.
    pypy_bin   = "~/FrameworkBenchmarks/installs/pypy/bin"
    python_bin = "~/FrameworkBenchmarks/installs/py2/bin"
    python3_bin= "~/FrameworkBenchmarks/installs/py3/bin"
    def easy_install(pkg, two=True, three=False, pypy=False):
      cmd = "/easy_install -ZU '" + pkg + "'"
      if two:   self.__run_command(python_bin + cmd, retry=True)
      if three: self.__run_command(python3_bin + cmd, retry=True)
      if pypy:  self.__run_command(pypy_bin + cmd, retry=True)

    self.__download("https://bitbucket.org/pypy/pypy/downloads/pypy-2.2-linux64.tar.bz2")
    self.__run_command("tar xjf pypy-2.2-linux64.tar.bz2")
    self.__run_command('ln -sf pypy-2.2-linux64 pypy')
    self.__download("http://www.python.org/ftp/python/2.7.6/Python-2.7.6.tgz")
    self.__run_command("tar xzf Python-2.7.6.tgz")
    self.__download("http://www.python.org/ftp/python/3.3.2/Python-3.3.2.tar.xz")
    self.__run_command("tar xJf Python-3.3.2.tar.xz")
    self.__run_command("./configure --prefix=$HOME/FrameworkBenchmarks/installs/py2 --disable-shared CC=gcc-4.8", cwd="Python-2.7.6")
    self.__run_command("./configure --prefix=$HOME/FrameworkBenchmarks/installs/py3 --disable-shared CC=gcc-4.8", cwd="Python-3.3.2")
    self.__run_command("make -j2", cwd="Python-2.7.6")
    self.__run_command("make install", cwd="Python-2.7.6")
    self.__run_command("make -j2", cwd="Python-3.3.2")
    self.__run_command("make install", cwd="Python-3.3.2")

    self.__download("https://bitbucket.org/pypa/setuptools/downloads/ez_setup.py")
    self.__run_command(pypy_bin + "/pypy ez_setup.py")
    self.__run_command(python_bin + "/python ez_setup.py")
    self.__run_command(python3_bin + "/python3 ez_setup.py")

    easy_install('pip==1.4.1', two=True, three=True, pypy=True)
    easy_install('MySQL-python==1.2.4', two=True, three=False, pypy=True)
    easy_install('https://github.com/clelland/MySQL-for-Python-3/archive/master.zip', two=False, three=True, pypy=False)
    easy_install('PyMySQL==0.6.1', two=True, three=True, pypy=True)
    easy_install('psycopg2==2.5.1', three=True)

    easy_install('simplejson==3.3.1', two=True, three=True, pypy=False)
    easy_install('ujson==1.33', three=True)
    easy_install('gevent==1.0')
    easy_install('uwsgi', three=True)  # uwsgi is released too often to stick on single version.

    # Gunicorn
    easy_install('gunicorn==18', two=True, three=True, pypy=True)
    # meinheld HEAD supports gunicorn worker on Python 3
    easy_install('https://github.com/mopemope/meinheld/archive/master.zip', two=True, three=True, pypy=True)

    # Tornado
    easy_install('tornado==3.1', two=True, three=True, pypy=True)
    easy_install('motor==0.1.2', two=True, three=True, pypy=True)
    easy_install('pymongo==2.5.2', two=True, three=True, pypy=True)

    # Django
    easy_install("https://www.djangoproject.com/download/1.6/tarball/", two=True, three=True, pypy=True)

    # Flask
    easy_install('Werkzeug==0.9.4', two=True, three=True, pypy=True)
    easy_install('flask==0.10.1', two=True, three=True, pypy=True)
    easy_install('sqlalchemy==0.8.3', two=True, three=False, pypy=True)
    # SQLAlchemy 0.9 supports C extension for Python 3
    easy_install('https://bitbucket.org/zzzeek/sqlalchemy/downloads/SQLAlchemy-0.9.0b1.tar.gz', two=False, three=True)
    easy_install('Jinja2==2.7.1', two=True, three=True, pypy=True)
    easy_install('Flask-SQLAlchemy==1.0', two=True, three=True, pypy=True)

    # Bottle
    easy_install('bottle==0.11.6', two=True, three=True, pypy=True)
    easy_install('bottle-sqlalchemy==0.4', two=True, three=True, pypy=True)

    # Falcon
    easy_install('Cython==0.19.2', two=True, three=True, pypy=True)
    easy_install('falcon==0.1.7', two=True, three=True, pypy=True)

  ############################################################
  # __install_error
  ############################################################
  def __install_error(self, message):
    print("\nINSTALL ERROR: %s\n" % message)
    if self.benchmarker.install_error_action == 'abort':
      sys.exit("Installation aborted.")
  ############################################################
  # End __install_error
  ############################################################

  ############################################################
  # __install_database_software
  ############################################################
  def __install_database_software(self):
    print("\nINSTALL: Installing database software\n")

    self.__run_command("cd .. && " + self.benchmarker.database_sftp_string(batch_file="config/database_sftp_batch"), True)

    remote_script = """

    ##############################
    # Prerequisites
    ##############################
    yes | sudo apt-get update
    yes | sudo apt-get install build-essential git libev-dev libpq-dev libreadline6-dev postgresql
    sudo sh -c "echo '*               -    nofile          65535' >> /etc/security/limits.conf"

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
    sudo cp usr.sbin.mysqld /etc/apparmor.d/
    sudo /etc/init.d/apparmor reload
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
    sudo mv 60-postgresql-shm.conf /etc/sysctl.d/60-postgresql-shm.conf

    ##############################
    # MongoDB
    ##############################
    sudo apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv 7F0CEB10
    sudo cp 10gen.list /etc/apt/sources.list.d/10gen.list
    sudo apt-get update
	yes | sudo apt-get remove mongodb-clients
    yes | sudo apt-get install mongodb-10gen

    sudo stop mongodb
    sudo mv /etc/mongodb.conf /etc/mongodb.conf.orig
    sudo mv mongodb.conf /etc/mongodb.conf
    sudo cp -R -p /var/lib/mongodb /ssd/
    sudo cp -R -p /var/log/mongodb /ssd/log/
    sudo start mongodb
    """
    
    print("\nINSTALL: %s" % self.benchmarker.database_ssh_string)
    p = subprocess.Popen(self.benchmarker.database_ssh_string.split(" "), stdin=subprocess.PIPE)
    p.communicate(remote_script)
    returncode = p.returncode
    if returncode != 0:
      self.__install_error("status code %s running subprocess '%s'." % (returncode, self.benchmarker.database_ssh_string))

    print("\nINSTALL: Finished installing database software\n")
  ############################################################
  # End __install_database_software
  ############################################################

  ############################################################
  # __install_client_software
  ############################################################
  def __install_client_software(self):
    print("\nINSTALL: Installing client software\n")

    remote_script = """

    ##############################
    # Prerequisites
    ##############################
    yes | sudo apt-get update
    yes | sudo apt-get install build-essential git libev-dev libpq-dev libreadline6-dev postgresql
    sudo sh -c "echo '*               -    nofile          65535' >> /etc/security/limits.conf"

    sudo mkdir -p /ssd
    sudo mkdir -p /ssd/log

    ##############################
    # wrk
    ##############################

    git clone https://github.com/wg/wrk.git
    cd wrk
    make
    sudo cp wrk /usr/local/bin
    cd ~

    git clone https://github.com/wg/wrk.git wrk-pipeline
    cd wrk-pipeline
    git checkout pipeline
    make
    sudo cp wrk /usr/local/bin/wrk-pipeline
    cd ~
    """
    
    print("\nINSTALL: %s" % self.benchmarker.client_ssh_string)
    p = subprocess.Popen(self.benchmarker.client_ssh_string.split(" "), stdin=subprocess.PIPE)
    p.communicate(remote_script)
    returncode = p.returncode
    if returncode != 0:
      self.__install_error("status code %s running subprocess '%s'." % (returncode, self.benchmarker.client_ssh_string))

    print("\nINSTALL: Finished installing client software\n")
  ############################################################
  # End __install_client_software
  ############################################################

  ############################################################
  # __run_command
  ############################################################
  def __run_command(self, command, send_yes=False, cwd=None, retry=False):
    try:
      cwd = os.path.join(self.install_dir, cwd)
    except AttributeError:
      cwd = self.install_dir

    if retry:
      max_attempts = 5
    else:
      max_attempts = 1
    attempt = 1
    delay = 0

    print("\nINSTALL: %s (cwd=%s)" % (command, cwd))

    while True:
      error_message = ""
      try:
        # Execute command.
        if send_yes:
          process = subprocess.Popen(command, shell=True, stdin=subprocess.PIPE, cwd=cwd)
          process.communicate("yes")
          returncode = process.returncode
          if returncode:
            raise subprocess.CalledProcessError(returncode, command)
        else:
          subprocess.check_call(command, shell=True, cwd=cwd)
        break  # Exit loop if successful.
      except:
        exceptionType, exceptionValue, exceptionTraceBack = sys.exc_info()
        error_message = "".join(traceback.format_exception_only(exceptionType, exceptionValue))
        print error_message

      # Exit if there are no more attempts left.
      attempt += 1
      if attempt > max_attempts:
        break

      # Delay before next attempt.
      if delay == 0:
        delay = 5
      else:
        delay = delay * 2
      print("Attempt %s/%s starting in %s seconds." % (attempt, max_attempts, delay))
      time.sleep(delay)

    if error_message:
      self.__install_error(error_message)
  ############################################################
  # End __run_command
  ############################################################

  ############################################################
  # __bash_from_string
  # Runs bash -c "command" in install_dir.
  ############################################################
  def __bash_from_string(self, command):
    self.__run_command('bash -c "%s"' % command)
  ############################################################
  # End __bash_from_string
  ############################################################

  ############################################################
  # __download
  # Downloads a file from a URI.
  ############################################################
  def __download(self, uri, filename=""):
    if filename:
      filename_option = "-O %s " % filename
    else:
      filename_option = ""
    command = "wget -nv --no-check-certificate --trust-server-names %s%s" % (filename_option, uri)
    self.__run_command(command, retry=True)
  ############################################################
  # End __download
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

# vim: sw=2
