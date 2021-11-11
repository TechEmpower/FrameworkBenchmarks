FROM buildpack-deps:bionic

ADD create.sql create.sql
ADD my.cnf my.cnf
ADD mysql.list mysql.list

RUN cp mysql.list /etc/apt/sources.list.d/
RUN apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys 8C718D3B5072E1F5

RUN apt-get update > /dev/null
RUN apt-get install -yqq locales > /dev/null

RUN locale-gen en_US.UTF-8
ENV LANG en_US.UTF-8
ENV LANGUAGE en_US:en
ENV LC_ALL en_US.UTF-8

# https://bugs.mysql.com/bug.php?id=90695
RUN ["/bin/bash", "-c", "debconf-set-selections <<< \"mysql-server mysql-server/lowercase-table-names select Enabled\""]
RUN ["/bin/bash", "-c", "debconf-set-selections <<< \"mysql-community-server mysql-community-server/data-dir select 'Y'\""]
RUN ["/bin/bash", "-c", "debconf-set-selections <<< \"mysql-community-server mysql-community-server/root-pass password secret\""]
RUN ["/bin/bash", "-c", "debconf-set-selections <<< \"mysql-community-server mysql-community-server/re-root-pass password secret\""]
RUN echo "Installing mysql-server version: $(apt-cache policy mysql-server | grep -oP "(?<=Candidate: )(.*)$")"
RUN DEBIAN_FRONTEND=noninteractive apt-get -y install mysql-server > /dev/null

RUN mv /etc/mysql/my.cnf /etc/mysql/my.cnf.orig
RUN cp my.cnf /etc/mysql/my.cnf

RUN rm -rf /ssd/mysql
RUN rm -rf /ssd/log/mysql
RUN cp -R -p /var/lib/mysql /ssd/
RUN cp -R -p /var/log/mysql /ssd/log
RUN mkdir -p /var/run/mysqld

RUN chown -R mysql:mysql /var/lib/mysql /var/log/mysql /var/run/mysqld /ssd && \
    (mysqld &) && \
    until mysqladmin -uroot -psecret ping; do sleep 1; done && \
    mysqladmin -uroot -psecret flush-hosts && \
    mysql -uroot -psecret < create.sql && \
    mysqladmin -uroot -psecret shutdown && \
    chown -R mysql:mysql /var/lib/mysql /var/log/mysql /var/run/mysqld /ssd

CMD ["mysqld"]
