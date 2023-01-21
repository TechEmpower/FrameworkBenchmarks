FROM buildpack-deps:jammy

ARG MONGODB_VERSION=6.0

COPY ./ ./

ARG DEBIAN_FRONTEND=noninteractive
RUN wget -qO - https://www.mongodb.org/static/pgp/server-${MONGODB_VERSION}.asc > /etc/apt/keyrings/mongodb-org.asc
RUN echo "deb [ signed-by=/etc/apt/keyrings/mongodb-org.asc ] https://repo.mongodb.org/apt/ubuntu jammy/mongodb-org/$MONGODB_VERSION multiverse" > \
      /etc/apt/sources.list.d/mongodb-org.list
# Complete and utter hax if works
RUN ln -s /bin/echo /bin/systemctl
RUN apt-get -yqq update && apt-get -yqq install mongodb-org

RUN mkdir -p /data/db
RUN chmod 777 /data/db

RUN mongod --fork --logpath /var/log/mongodb.log --bind_ip_all && sleep 10 && mongosh < create.js && sleep 10

CMD ["mongod", "--bind_ip_all"]
