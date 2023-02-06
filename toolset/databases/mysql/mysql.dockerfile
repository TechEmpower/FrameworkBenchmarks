FROM buildpack-deps:jammy

ARG MYSQL_VERSION=8.0

ADD create.sql create.sql
ADD my.cnf my.cnf

ARG DEBIAN_FRONTEND=noninteractive
RUN wget -qO - "https://keyserver.ubuntu.com/pks/lookup?op=get&search=0x859be8d7c586f538430b19c2467b942d3a79bd29" > \
      /etc/apt/keyrings/mysql.asc
RUN echo "deb [ signed-by=/etc/apt/keyrings/mysql.asc ] http://repo.mysql.com/apt/ubuntu jammy mysql-${MYSQL_VERSION}" > \
      /etc/apt/sources.list.d/mysql.list
RUN apt-get -yqq update && apt-get -yqq install apt-utils locales

RUN locale-gen en_US.UTF-8
ENV LANG=en_US.UTF-8
ENV LANGUAGE=en_US:en
ENV LC_ALL=en_US.UTF-8

# https://bugs.mysql.com/bug.php?id=90695
RUN ["/bin/bash", "-c", "debconf-set-selections <<< \"mysql-server mysql-server/lowercase-table-names select Enabled\""]
RUN ["/bin/bash", "-c", "debconf-set-selections <<< \"mysql-community-server mysql-community-server/data-dir select 'Y'\""]
RUN ["/bin/bash", "-c", "debconf-set-selections <<< \"mysql-community-server mysql-community-server/root-pass password secret\""]
RUN ["/bin/bash", "-c", "debconf-set-selections <<< \"mysql-community-server mysql-community-server/re-root-pass password secret\""]
RUN apt-get -yqq install mysql-server

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
