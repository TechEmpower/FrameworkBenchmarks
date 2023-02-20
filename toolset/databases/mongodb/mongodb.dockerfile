FROM ubuntu:22.04

ARG MONGODB_VERSION=6.0

COPY create.js /tmp/

ARG DEBIAN_FRONTEND=noninteractive
ADD "https://www.mongodb.org/static/pgp/server-${MONGODB_VERSION}.asc" \
    /etc/apt/keyrings/mongodb-org.asc
RUN apt-get -yqq update && \
    apt-get -yqq install \
      apt-utils \
      ca-certificates \
      lsb-release && \
    chmod 644 /etc/apt/keyrings/mongodb-org.asc && \
    echo "deb [ signed-by=/etc/apt/keyrings/mongodb-org.asc ] https://repo.mongodb.org/apt/ubuntu $(lsb_release -cs)/mongodb-org/${MONGODB_VERSION} multiverse" > \
      /etc/apt/sources.list.d/mongodb-org.list && \
    apt-get -yqq update && \
    # Complete and utter hax if it works
    ln -s /bin/echo /bin/systemctl && \
    apt-get -yqq install mongodb-org && \
    install -dm777 /data/db && \
    mongod --fork --logpath /var/log/mongodb.log --bind_ip_all && \
    sleep 10 && \
    mongosh < /tmp/create.js && \
    sleep 10

CMD ["mongod", "--bind_ip_all"]
