FROM node:20.16-slim

COPY ./ ./

RUN npm install

ENV NODE_ENV production
ENV DATABASE mysql
ENV MYSQL_HOST tfb-database
ENV MYSQL_USER benchmarkdbuser
ENV MYSQL_PSWD benchmarkdbpass
ENV MYSQL_DBNAME hello_world

EXPOSE 8080

CMD ["node", "app.js"]
