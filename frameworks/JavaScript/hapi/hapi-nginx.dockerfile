FROM node:10.10.0

RUN apt update
RUN apt install nginx -y

WORKDIR /

COPY ./ ./
RUN chmod +x start-servers.sh

ENV NODE_HANDLER sequelize-postgres

RUN npm install

CMD ./start-servers.sh && nginx -c /nginx.conf -g "daemon off;"
