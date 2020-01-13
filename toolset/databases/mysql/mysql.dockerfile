FROM mysql:8

EXPOSE 3306

ENV MYSQL_DATABASE=hello_world
ENV MYSQL_USER=benchmarkdbuser
ENV MYSQL_PASSWORD=benchmarkdbpass
ENV MYSQL_ALLOW_EMPTY_PASSWORD=1

ADD create.sql /docker-entrypoint-initdb.d/
ADD my.cnf /etc/mysql/conf.d/
