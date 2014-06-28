import subprocess
import os
import os.path
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
    self.__run_command("sudo apt-get -y update")
    self.__run_command("sudo apt-get -y upgrade")
    self.__run_command("sudo apt-get install build-essential libpcre3 libpcre3-dev libpcrecpp0 libssl-dev zlib1g-dev python-software-properties unzip git-core libcurl4-openssl-dev libbz2-dev libmysqlclient-dev mongodb-clients libreadline6-dev libyaml-dev libsqlite3-dev sqlite3 libxml2-dev libxslt-dev libgdbm-dev ncurses-dev automake libffi-dev htop libtool bison libevent-dev libgstreamer-plugins-base0.10-0 libgstreamer0.10-0 liborc-0.4-0 libwxbase2.8-0 libwxgtk2.8-0 libgnutls-dev libjson0-dev libmcrypt-dev libicu-dev cmake gettext curl libpq-dev mercurial mlton", True)
    self.__run_command("sudo add-apt-repository ppa:ubuntu-toolchain-r/test -y")
    self.__run_command("sudo apt-get -y update")
    self.__run_command("sudo apt-get install -y gcc-4.8 g++-4.8")

    self.__run_command("cp ../config/benchmark_profile ../../.bash_profile")
    self.__run_command("cat ../config/benchmark_profile >> ../../.profile")
    self.__run_command("cat ../config/benchmark_profile >> ../../.bashrc")
    self.__run_command(". ../../.profile")
    self.__run_command("sudo sh -c \"echo '*               -    nofile          65535' >> /etc/security/limits.conf\"")

    ##############################################################
    # System Tools
    ##############################################################

    #
    # Leiningen
    #
    if not self.__path_exists("bin/lein"):
        self.__run_command("mkdir -p bin")
        self.__download("https://raw.github.com/technomancy/leiningen/stable/bin/lein")
        self.__run_command("mv lein bin/lein")
        self.__run_command("chmod +x bin/lein")

    #
    # Maven
    #
    if not self.__path_exists("/usr/bin/mvn"):
        self.__run_command("sudo apt-get install maven -qq")
        self.__run_command("mvn -version")

    #######################################
    # Languages
    #######################################
    self._install_python()

    #
    # Dart
    #
    if not self.__path_exists("dart-sdk"):
        self.__download("http://storage.googleapis.com/dart-archive/channels/stable/release/latest/sdk/dartsdk-linux-x64-release.zip")
        self.__run_command("unzip dartsdk-linux-x64-release.zip")

    #
    # Erlang
    #
    if not self.__path_exists("/usr/bin/erl"):
        self.__run_command("sudo cp ../config/erlang.list /etc/apt/sources.list.d/erlang.list")
        self.__download("http://binaries.erlang-solutions.com/debian/erlang_solutions.asc")
        self.__run_command("sudo apt-key add erlang_solutions.asc")
        self.__run_command("sudo apt-get update")
        self.__run_command("sudo apt-get install -y esl-erlang")

    #
    # nodejs
    #
    if not self.__path_exists("node-v0.10.8-linux-x64"):
        self.__download("http://nodejs.org/dist/v0.10.8/node-v0.10.8-linux-x64.tar.gz")
        self.__run_command("tar xzf node-v0.10.8-linux-x64.tar.gz")

    #
    # Java
    #
    self.__run_command("sudo apt-get install -y openjdk-7-jdk")
    self.__run_command("sudo apt-get remove --purge openjdk-6-jre openjdk-6-jre-headless", True)

    #
    # Elixir
    #
    if not self.__path_exists("v0.13.3.tar.gz"):
      self.__run_command("wget https://github.com/elixir-lang/elixir/archive/v0.13.3.tar.gz");
    self.__run_command("sudo tar -zxf v0.13.3.tar.gz");
    #self.__run_command("sudo make clean", cwd="elixir-0.13.3");
    self.__run_command("sudo make test", cwd="elixir-0.13.3");
    self.__run_command("sudo make clean", cwd="elixir-0.13.3");

    #
    # Ruby/JRuby
    #
    self.__run_command("curl -L get.rvm.io | bash -s head --auto-dotfiles")
    self.__run_command("echo rvm_auto_reload_flag=2 >> ~/.rvmrc")
    if not self.__path_exists("../../.rvm/rubies/ruby-2.0.0-p0/"):
      self.__bash_from_string("source ~/.rvm/scripts/'rvm' && rvm install 2.0.0-p0")
      self.__bash_from_string("source ~/.rvm/scripts/'rvm' && rvm 2.0.0-p0 do gem install bundler")
    if not self.__path_exists("../../.rvm/rubies/jruby-1.7.8/"):
      self.__bash_from_string("source ~/.rvm/scripts/'rvm' && rvm install jruby-1.7.8")
      self.__bash_from_string("source ~/.rvm/scripts/'rvm' && rvm jruby-1.7.8 do gem install bundler")

    #
    # go
    #
    if not self.__path_exists("go"):
        self.__download("http://go.googlecode.com/files/go1.2.linux-amd64.tar.gz");
        self.__run_command("tar xzf go1.2.linux-amd64.tar.gz")

    #
    # Perl
    #
    if not self.__path_exists("/opt/ActivePerl-5.16/bin/perl"):
        self.__download("http://downloads.activestate.com/ActivePerl/releases/5.16.3.1603/ActivePerl-5.16.3.1603-x86_64-linux-glibc-2.3.5-296746.tar.gz");
        self.__run_command("tar xzf ActivePerl-5.16.3.1603-x86_64-linux-glibc-2.3.5-296746.tar.gz");
        self.__run_command("sudo ./install.sh --license-accepted --prefix /opt/ActivePerl-5.16 --no-install-html", cwd="ActivePerl-5.16.3.1603-x86_64-linux-glibc-2.3.5-296746", send_yes=True, retry=True)

    self.__download("http://cpanmin.us", "cpanminus.pl")
    self.__run_command("perl cpanminus.pl --sudo App::cpanminus", retry=True)
    self.__run_command("cpanm -n -S DBI DBD::mysql Kelp Dancer Mojolicious Kelp::Module::JSON::XS Dancer::Plugin::Database Starman Plack JSON Web::Simple DBD::Pg JSON::XS EV HTTP::Parser::XS Monoceros EV IO::Socket::IP IO::Socket::SSL", retry=True)

    #
    # php
    #
    if not self.__path_exists("/usr/local/bin/php"):
        self.__download("http://museum.php.net/php5/php-5.4.13.tar.gz")
        self.__run_command("tar xzf php-5.4.13.tar.gz")
        self.__run_command("./configure --with-pdo-mysql --with-mysql --with-mcrypt --enable-intl --enable-mbstring --enable-fpm --with-fpm-user=www-data --with-fpm-group=www-data --with-openssl", cwd="php-5.4.13")
        self.__run_command("make", cwd="php-5.4.13")
        self.__run_command("sudo make install", cwd="php-5.4.13")

    if not self.__path_exists("/usr/local/lib/php/extensions/no-debug-non-zts-20100525/apc.so"):
        self.__run_command("printf \"\\n\" | sudo pecl install apc-beta", cwd="php-5.4.13", retry=True)

    self.__run_command("sudo cp ../config/php.ini /usr/local/lib/php.ini")
    self.__run_command("sudo cp ../config/php-fpm.conf /usr/local/lib/php-fpm.conf")

    # Composer
    if not self.__path_exists("bin/composer.phar"):
        self.__download("https://getcomposer.org/installer", "composer-installer.php")
        self.__run_command("php composer-installer.php --install-dir=bin")

    # Phalcon
    if not self.__path_exists("/usr/local/lib/php/extensions/no-debug-non-zts-20100525/phalcon.so"):
        self.__run_command("test -d cphalcon || git clone git://github.com/phalcon/cphalcon.git", retry=True)
        self.__run_command("sudo ./install", cwd="cphalcon/build")

    # YAF
    if not self.__path_exists("/usr/local/lib/php/extensions/no-debug-non-zts-20100525/yaf.so"):
        self.__run_command("sudo pecl install -f yaf")

    #
    # Haskell
    #
    if not self.__path_exists("/usr/bin/haskell-compiler"):       ##not sure if right
      self.__run_command("sudo apt-get install -y ghc cabal-install")

    #
    # RingoJs
    #
    if not self.__path_exists("/usr/share/ringojs"):
        self.__download("http://www.ringojs.org/downloads/ringojs_0.10-1_all.deb")
        self.__run_command("sudo apt-get install -y jsvc")
        self.__run_command("sudo dpkg -i ringojs_0.10-1_all.deb", True)
        self.__run_command("rm ringojs_0.10-1_all.deb")

    #
    # Mono
    #
    if not self.__path_exists("/usr/local/bin/mono"):
        self.__run_command("test -d mono || git clone git://github.com/mono/mono", retry=True)
        self.__run_command("git checkout mono-3.2.8-branch", cwd="mono")
        self.__run_command("./autogen.sh --prefix=/usr/local", cwd="mono")
        self.__run_command("make get-monolite-latest", cwd="mono")
        self.__run_command("make EXTERNAL_MCS=${PWD}/mcs/class/lib/monolite/basic.exe", cwd="mono")
        self.__run_command("sudo make install", cwd="mono")

    self.__run_command("mozroots --import --sync", retry=True)

    if not self.__path_exists("/usr/local/bin/xsp"):
        self.__run_command("test -d xsp || git clone git://github.com/mono/xsp", retry=True)
        self.__run_command("./autogen.sh --prefix=/usr/local", cwd="xsp")
        self.__run_command("make", cwd="xsp")
        self.__run_command("sudo make install", cwd="xsp")

    #
    # Nimrod
    #
    if not self.__path_exists("nimrod/bin/nimrod"):
        self.__run_command("test -d nimrod || git clone git://github.com/Araq/Nimrod.git nimrod", retry=True)
        self.__run_command("git checkout 987ac2439a87d74838233a7b188e4db340495ee5", cwd="nimrod")
        self.__run_command("test -d csources || git clone git://github.com/nimrod-code/csources.git", cwd="nimrod", retry=True)
        self.__run_command("git checkout 704015887981932c78a033dd5ede623b2ad6ae27", cwd="nimrod/csources");
        self.__run_command("chmod +x build.sh", cwd="nimrod/csources")
        self.__run_command("./build.sh", cwd="nimrod/csources")

    if not self.__path_exists("nimrod/koch"):
        self.__run_command("bin/nimrod c koch", cwd="nimrod")
    self.__run_command("./koch boot -d:release", cwd="nimrod")

    #
    # Racket
    #
    if not self.__path_exists("racket-5.3.6/bin/racket") or not self.__path_exists("racket-5.3.6/collects/racket"):
        self.__download("https://github.com/plt/racket/archive/v5.3.6.tar.gz", "racket-5.3.6.tar.gz")
        self.__run_command("tar xzf racket-5.3.6.tar.gz")
        self.__run_command("./configure", cwd="racket-5.3.6/src")
        self.__run_command("make", cwd="racket-5.3.6/src")
        self.__run_command("sudo make install", cwd="racket-5.3.6/src")

    #
    # Ur/Web
    #
    if not self.__path_exists("/usr/local/bin/urweb"):
        self.__run_command("hg clone http://hg.impredicative.com/urweb")
        self.__run_command("./autogen.sh", cwd="urweb")
        self.__run_command("./configure", cwd="urweb")
        self.__run_command("make", cwd="urweb")
        self.__run_command("sudo make install", cwd="urweb")
    
    #
    # HHVM
    #
    if not self.__path_exists("/usr/bin/hhvm"):
        self.__run_command("sudo add-apt-repository ppa:mapnik/v2.2.0", True)
        self.__run_command("wget -O - http://dl.hhvm.com/conf/hhvm.gpg.key | sudo apt-key add -")
        self.__run_command("echo deb http://dl.hhvm.com/ubuntu trusty main | sudo tee /etc/apt/sources.list.d/hhvm.list")
        self.__run_command("sudo apt-get update")
        self.__run_command("sudo apt-get install -y hhvm")

    #######################################
    # Webservers
    #######################################

    #
    # Nginx
    #
    if not self.__path_exists("/usr/local/nginx/sbin/nginx"):
        self.__download("http://nginx.org/download/nginx-1.4.1.tar.gz")
        self.__run_command("tar xzf nginx-1.4.1.tar.gz")
        self.__run_command("./configure", cwd="nginx-1.4.1")
        self.__run_command("make", cwd="nginx-1.4.1")
        self.__run_command("sudo make install", cwd="nginx-1.4.1")

    #
    # Openresty (nginx with lua stuff)
    #
    if not self.__path_exists("/usr/local/openresty/nginx/sbin/nginx"):
        self.__download("http://openresty.org/download/ngx_openresty-1.5.8.1.tar.gz")
        self.__run_command("tar xzf ngx_openresty-1.5.8.1.tar.gz")
        self.__run_command("./configure --with-luajit --with-http_postgres_module", cwd="ngx_openresty-1.5.8.1")
        self.__run_command("make", cwd="ngx_openresty-1.5.8.1")
        self.__run_command("sudo make install", cwd="ngx_openresty-1.5.8.1")
    
    #
    # Lapis
    #
    if not self.__path_exists("/usr/local/bin/lapis"):
        self.__run_command("sudo apt-get install -y luarocks")
        self.__run_command("sudo luarocks install http://github.com/leafo/lapis/raw/master/lapis-dev-1.rockspec")


    #
    # Resin
    #
    if not self.__path_exists("resin-4.0.36/conf/resin.xml"):
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
    if not self.__path_exists("/usr/local/lib/libzmq.a"):
        self.__download("http://download.zeromq.org/zeromq-4.0.3.tar.gz")
        self.__run_command("tar xzf zeromq-4.0.3.tar.gz")
        self.__run_command("./configure", cwd="zeromq-4.0.3")
        self.__run_command("make", cwd="zeromq-4.0.3")
        self.__run_command("sudo make install", cwd="zeromq-4.0.3")

    self.__run_command("sudo apt-get install -y sqlite3 libsqlite3-dev uuid uuid-runtime uuid-dev")
    self.__run_command("sudo ldconfig -v")

    if not self.__path_exists("/usr/local/bin/mongrel2"):
        self.__download("https://github.com/zedshaw/mongrel2/tarball/v1.8.1", "mongrel2.tar.gz")
        self.__run_command("tar xvf mongrel2.tar.gz")
        self.__run_command("mv zedshaw-mongrel2-aa2ecf8 mongrel2")
        # for zmq4, we update the following file manually (not in v1.8.1)
        self.__download("https://raw.github.com/zedshaw/mongrel2/9b565eeea003783c47502c2d350b99c9684ce97c/src/zmq_compat.h")
        self.__run_command("mv -f zmq_compat.h mongrel2/src/")
        self.__run_command("make clean all && sudo make install", cwd="mongrel2")

    #
    # Weber
    #
    #self.__run_command("git clone https://github.com/elixir-web/weber.git");
    # To get the two make commands working, we need to hard code the path for elixir's "mix"
    #self.__run_command("sed -i 's:$(MIX):/home/tfb/FrameworkBenchmarks/installs/elixir-0.13.3/bin/mix:' Makefile", cwd="weber")
    #self.__run_command("bash -i -c 'make'", cwd="weber");
    #self.__run_command("bash -i -c 'sudo make test'", cwd="weber");

    ##############################################################
    # Frameworks
    ##############################################################

    #
    # Grails
    #
    if not self.__path_exists("grails-2.4.1"):
        self.__download("http://dist.springframework.org.s3.amazonaws.com/release/GRAILS/grails-2.4.1.zip")
        self.__run_command("unzip -o grails-2.4.1.zip")

    #
    # Play 2
    #
    if not self.__path_exists("play-2.2.0"):
        self.__download("http://downloads.typesafe.com/play/2.2.0/play-2.2.0.zip")
        self.__run_command("unzip -o play-2.2.0.zip")

    #
    # Play 1
    #
    if not self.__path_exists("play-1.2.5"):
        self.__download("http://downloads.typesafe.com/releases/play-1.2.5.zip")
        self.__run_command("unzip -o play-1.2.5.zip")
        self.__run_command("mv play-1.2.5/play play-1.2.5/play1")

    # siena
    self.__run_command("yes | play-1.2.5/play1 install siena")

    #
    # TreeFrog Framework
    #
    if not self.__path_exists("/usr/bin/treefrog") or not self.__path_exists("/usr/bin/tspawn"):
        self.__run_command("sudo apt-get install -y qt4-qmake libqt4-dev libqt4-sql-mysql libqt4-sql-psql g++")
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
    if not self.__path_exists("vert.x-2.1RC3"):
        self.__download("http://dl.bintray.com/vertx/downloads/vert.x-2.1RC3.tar.gz?direct=true", "vert.x-2.1RC3.tar.gz")
        self.__run_command("tar xzf vert.x-2.1RC3.tar.gz")

    #
    # Yesod
    #
    self.__run_command("cabal update", retry=True)
    self.__run_command("cabal install yesod persistent-mysql", retry=True)

    #
    # Jester
    #
    if not self.__path_exists("jester"):
        self.__run_command("git clone git://github.com/dom96/jester.git jester/jester", retry=True)

    #
    # Onion
    #
    if not self.__path_exists("onion"):
        self.__run_command("git clone https://github.com/davidmoreno/onion.git")
        self.__run_command("mkdir build", cwd="onion")
        self.__run_command("cmake ..", cwd="onion/build")
        self.__run_command("make", cwd="onion/build")

    # nawak
    #
    if not self.__path_exists("nawak"):
        self.__run_command("git clone git://github.com/idlewan/nawak.git nawak/nawak", retry=True)

    self.__run_command("sudo apt-get -y autoremove || true");

    #
    # Wt
    #
    if not self.__path_exists("nawak"):
      self.__run_command("sudo apt-get install libboost1.54-all-dev")
      self.__download("http://downloads.sourceforge.net/witty/wt-3.3.3.tar.gz", filename="wt.tar.gz")
      self.__run_command("tar xf wt.tar.gz")
      self.__run_command("rm wt.tar.gz")
      self.__run_command("bash -c 'mv wt-* wt'")
      self.__run_command("mkdir build", cwd="wt")
      self.__run_command("cmake .. -DWT_CPP_11_MODE=-std=c++0x -DCMAKE_BUILD_TYPE=Release", cwd="wt/build")
      self.__run_command("make", cwd="wt/build")
      self.__run_command("sudo make install", cwd="wt/build")

    print("\nINSTALL: Finished installing server software\n")
  ############################################################
  # End __install_server_software
  ############################################################

  def _install_python(self):
    """Install Python runtime, frameworks and libraries"""
    # PyPy 2.3.1
    f = "pypy-2.3.1-linux64.tar.bz2"
    #if not os.path.exists(f):
    if not self.__path_exists("pypy"):
      self.__download("https://bitbucket.org/pypy/pypy/downloads/" + f, f)
      self.__run_command("tar xf " + f)
      self.__run_command('ln -sf pypy-2.3.1-linux64 pypy')

    # CPython 2.7.7
    f = "Python-2.7.7.tgz"
    #if not os.path.exists("py2"):
    if not self.__path_exists("py2"):
      self.__download("http://www.python.org/ftp/python/2.7.7/" + f, f)
      self.__run_command("tar xf " + f)
      self.__run_command("./configure --prefix=$HOME/FrameworkBenchmarks/installs/py2 --disable-shared", cwd="Python-2.7.7")
      self.__run_command("make -j4", cwd="Python-2.7.7")
      self.__run_command("make install", cwd="Python-2.7.7")

    # CPython 3.4.1
    f = "Python-3.4.1.tar.xz"
    if not self.__path_exists("py3"):
    #if not os.path.exists("py3"):
      self.__download("https://www.python.org/ftp/python/3.4.1/" + f, f)
      self.__run_command("tar xf " + f)
      self.__run_command("./configure --prefix=$HOME/FrameworkBenchmarks/installs/py3 --disable-shared", cwd="Python-3.4.1")
      self.__run_command("make -j4", cwd="Python-3.4.1")
      self.__run_command("make install", cwd="Python-3.4.1")

    if not self.__path_exists("get-pip.py"):
    #if not os.path.exists("get-pip.py"):
      self.__download("https://bootstrap.pypa.io/get-pip.py", "get-pip.py")
      self.__run_command("py2/bin/python get-pip.py")
      self.__run_command("pypy/bin/pypy get-pip.py")
    # Python 3.4.1 installs pip by default.

    self.__run_command('py2/bin/pip install -r ../config/requirements.txt')
    self.__run_command('py3/bin/pip3 install -r ../config/requirements-py3.txt')
    self.__run_command('pypy/bin/pip install -r ../config/requirements-pypy.txt')

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
 
    self.__run_command("cd .. && " + self.benchmarker.database_sftp_string(batch_file="../config/database_sftp_batch"), True)

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

    yes | sudo apt-get install mysql-server-5.6

    sudo stop mysql
    # disable checking of disk size
    sudo cp mysql /etc/init.d/mysql
    sudo chmod +x /etc/init.d/mysql
    sudo cp mysql.conf /etc/init/mysql.conf
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
    sudo mv postgresql.conf /etc/postgresql/9.3/main/postgresql.conf
    sudo mv pg_hba.conf /etc/postgresql/9.3/main/pg_hba.conf

    sudo cp -R -p /var/lib/postgresql/9.3/main /ssd/postgresql
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
    yes | sudo apt-get install build-essential git libev-dev libpq-dev libreadline6-dev 
    sudo sh -c "echo '*               -    nofile          65535' >> /etc/security/limits.conf"


    ##############################
    # wrk
    ##############################

    git clone https://github.com/wg/wrk.git
    cd wrk
    make
    sudo cp wrk /usr/local/bin
    cd ~
    
    #############################
    # pipeline.lua
    #############################
cat << EOF | tee pipeline.lua
init = function(args)
  wrk.init(args)
  local r = {}
  local depth = tonumber(args[1]) or 1
  for i=1,depth do
    r[i] = wrk.format()
  end
  req = table.concat(r)
end

request = function()
  return req
end
EOF
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
  # __path_exists
  ############################################################
  def __path_exists(self, path, cwd=None):
    full_path = os.path.join(cwd or self.install_dir, path)

    if os.path.exists(full_path):
        print("\nEXISTS: %s " % full_path)
        return True

    print("\nNOT_EXISTS: %s" % full_path)
    return False
  ############################################################
  # End __path_exists
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
    if send_yes:
      command = "yes yes | " + command
        

    print("\nINSTALL: %s (cwd=%s)" % (command, cwd))

    while attempt <= max_attempts:
      error_message = ""
      try:
	##print "working in " + cwd + " on " + command
        # Execute command.
        """if command == "make clean":
          print "make clean if statment"
	  time.sleep(5)
	  subprocess.check_call("make clean", shell=False, cwd=cwd)
	  break"""
        subprocess.check_call(command, shell=True, cwd=cwd)
        """if send_yes:
          process = subprocess.Popen(["/bin/bash", "-c", command], shell=False, stdin=subprocess.PIPE, cwd=cwd)
          process.communicate("yes")
          returncode = process.returncode
          if returncode:
            raise subprocess.CalledProcessError(returncode, command)
        else:
          subprocess.check_call(["/bin/bash", "-c", command], shell=False, cwd=cwd)"""
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
