FROM ubuntu:16.04

COPY ./ ./

RUN apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv 2930ADAE8CAF5059EE73BB4B58712A2291FA4AD5
RUN echo "deb [ arch=amd64,arm64 ] http://repo.mongodb.org/apt/ubuntu xenial/mongodb-org/3.6 multiverse" | tee /etc/apt/sources.list.d/mongodb-org-3.6.list
RUN apt-get -y update > /dev/null
RUN apt-get -y install mongodb-org > /dev/null

RUN mkdir -p /data/db
RUN chmod 777 /data/db

RUN mongod --fork --logpath /var/log/mongodb.log --bind_ip_all && sleep 10 && mongo < create.js && sleep 10

CMD ["mongod", "--bind_ip_all"]
