FROM mysql:9.0

ENV MYSQL_ROOT_PASSWORD=root
ENV MYSQL_USER=benchmarkdbuser
ENV MYSQL_PASSWORD=benchmarkdbpass
ENV MYSQL_DATABASE=hello_world

COPY my.cnf /etc/mysql/
COPY create.sql /docker-entrypoint-initdb.d/

COPY 60-database-shm.conf /etc/sysctl.d/60-database-shm.conf
