FROM buildpack-deps:bionic

COPY ./ ./

RUN apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv 4B7C549A058F8B6B
RUN echo "deb https://repo.mongodb.org/apt/ubuntu bionic/mongodb-org/4.2 multiverse" | tee /etc/apt/sources.list.d/mongodb-org.list
RUN apt-get -yqq update > /dev/null
# Complete and utter hax if works
RUN ln -s /bin/echo /bin/systemctl
RUN DEBIAN_FRONTEND=noninteractive apt-get -yqq install apt-transport-https mongodb-org > /dev/null

RUN mkdir -p /data/db
RUN chmod 777 /data/db

RUN mongod --fork --logpath /var/log/mongodb.log --bind_ip_all && sleep 10 && mongo < create.js && sleep 10

CMD ["mongod", "--bind_ip_all"]
