FROM node:20.16-slim

COPY ./ ./

RUN npm install
RUN npm run build

ENV NODE_ENV production
ENV DATABASE mysql
ENV MYSQL_HOST tfb-database
ENV MYSQL_USER benchmarkdbuser
ENV MYSQL_PSWD benchmarkdbpass
ENV MYSQL_DBNAME hello_world

EXPOSE 8080
CMD node dist/main.js
