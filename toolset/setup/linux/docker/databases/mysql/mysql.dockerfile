FROM ubuntu:16.04

RUN apt-get update
RUN apt-get install -qqy locales

RUN locale-gen en_US.UTF-8
ENV LANG en_US.UTF-8  
ENV LANGUAGE en_US:en  
ENV LC_ALL en_US.UTF-8 

ADD create.sql create.sql
ADD my.cnf my.cnf
ADD mysql.list mysql.list

RUN cp mysql.list /etc/apt/sources.list.d/
RUN apt-key adv --keyserver keyserver.ubuntu.com --recv-keys 8C718D3B5072E1F5
RUN apt-get update
RUN ["/bin/bash", "-c", "debconf-set-selections <<< \"mysql-community-server mysql-community-server/data-dir select 'Y'\""]
RUN ["/bin/bash", "-c", "debconf-set-selections <<< \"mysql-community-server mysql-community-server/root-pass password secret\""]
RUN ["/bin/bash", "-c", "debconf-set-selections <<< \"mysql-community-server mysql-community-server/re-root-pass password secret\""]
RUN DEBIAN_FRONTEND=noninteractive apt-get -y install mysql-server

RUN mv /etc/mysql/my.cnf /etc/mysql/my.cnf.orig
RUN cp my.cnf /etc/mysql/my.cnf

RUN rm -rf /ssd/mysql
RUN rm -rf /ssd/log/mysql
RUN cp -R -p /var/lib/mysql /ssd/
RUN cp -R -p /var/log/mysql /ssd/log

# It may seem weird that we call `service mysql start` several times, but the RUN
# directive is a 1-time operation for building this image. Subsequent RUN calls
# do not see running processes from prior RUN calls; therefor, each command here
# that relies on the mysql server running will explicitly start the server and
# perform the work required.
RUN service mysql start && mysqladmin -uroot -psecret flush-hosts
RUN service mysql start && mysql -uroot -psecret < create.sql

EXPOSE 3306

CMD ["mysqld"]