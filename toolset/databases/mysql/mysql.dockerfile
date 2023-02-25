FROM ubuntu:22.04

ARG MYSQL_VERSION=8.0

COPY create.sql /tmp/
COPY my.cnf ./

ARG DEBIAN_FRONTEND=noninteractive
ADD "https://keyserver.ubuntu.com/pks/lookup?op=get&search=0x859be8d7c586f538430b19c2467b942d3a79bd29" \
    /etc/apt/keyrings/mysql.asc
RUN chmod 644 /etc/apt/keyrings/mysql.asc && \
    apt-get -yqq update && \
    apt-get -yqq install \
      apt-utils \
      locales \
      lsb-release && \
    echo "deb [ signed-by=/etc/apt/keyrings/mysql.asc ] http://repo.mysql.com/apt/ubuntu $(lsb_release -cs) mysql-${MYSQL_VERSION}" > \
      /etc/apt/sources.list.d/mysql.list && \
    locale-gen en_US.UTF-8

ENV LANG=en_US.UTF-8
ENV LANGUAGE=en_US:en
ENV LC_ALL=en_US.UTF-8

# https://bugs.mysql.com/bug.php?id=90695
RUN ["/bin/bash", "-c", "debconf-set-selections <<< \"mysql-server mysql-server/lowercase-table-names select Enabled\""]
RUN ["/bin/bash", "-c", "debconf-set-selections <<< \"mysql-community-server mysql-community-server/data-dir select 'Y'\""]
RUN ["/bin/bash", "-c", "debconf-set-selections <<< \"mysql-community-server mysql-community-server/root-pass password secret\""]
RUN ["/bin/bash", "-c", "debconf-set-selections <<< \"mysql-community-server mysql-community-server/re-root-pass password secret\""]
RUN apt-get -yqq update && \
    apt-get -yqq install mysql-server && \
    mv /etc/mysql/my.cnf /etc/mysql/my.cnf.orig && \
    mv my.cnf /etc/mysql/my.cnf && \
    rm -rf /ssd/log/mysql /ssd/mysql && \
    cp -Rp /var/lib/mysql /ssd  && \
    cp -Rp /var/log/mysql /ssd/log && \
    mkdir -p /var/run/mysqld && \
    chown -R mysql:mysql /ssd /var/lib/mysql /var/log/mysql /var/run/mysqld && \
    (mysqld &) && \
    until mysqladmin -uroot -psecret ping; do sleep 1; done && \
    mysqladmin -uroot -psecret flush-hosts && \
    mysql -uroot -psecret < /tmp/create.sql && \
    mysqladmin -uroot -psecret shutdown && \
    chown -R mysql:mysql /ssd /var/lib/mysql /var/log/mysql /var/run/mysqld

CMD ["mysqld"]
