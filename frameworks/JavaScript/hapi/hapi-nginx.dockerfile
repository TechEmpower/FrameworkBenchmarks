FROM node:12.3.1-slim

RUN apt-get update
RUN apt-get install nginx -y

WORKDIR /

COPY ./ ./
RUN chmod +x start-servers.sh
RUN chmod +x build-nginx-conf.sh

RUN ./build-nginx-conf.sh

ENV NODE_HANDLER sequelize-postgres

RUN npm install

CMD ./start-servers.sh && nginx -c /nginx.conf -g "daemon off;"
