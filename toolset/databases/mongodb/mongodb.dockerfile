FROM buildpack-deps:bionic

COPY ./ ./

RUN apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv 68818C72E52529D4
RUN echo "deb [ arch=amd64,arm64 ] https://repo.mongodb.org/apt/ubuntu bionic/mongodb-org/4.0 multiverse" | tee /etc/apt/sources.list.d/mongodb-org.list
RUN apt-get -yqq update > /dev/null
RUN DEBIAN_FRONTEND=noninteractive apt-get -yqq install apt-transport-https mongodb-org > /dev/null

RUN mkdir -p /data/db
RUN chmod 777 /data/db

RUN mongod --fork --logpath /var/log/mongodb.log --bind_ip_all && sleep 10 && mongo < create.js && sleep 10

CMD ["mongod", "--bind_ip_all"]
