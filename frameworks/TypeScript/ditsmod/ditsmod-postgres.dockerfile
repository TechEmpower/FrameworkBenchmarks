FROM node:20.16-slim

COPY ./ ./

RUN npm install
RUN npm run build

ENV NODE_ENV production
ENV DATABASE postgres
ENV PG_HOST tfb-database
ENV PG_USER benchmarkdbuser
ENV PG_PSWD benchmarkdbpass
ENV PG_DBNAME hello_world

EXPOSE 8080
CMD node dist/main.js
