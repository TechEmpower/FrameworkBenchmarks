FROM node:14.17.1-slim

WORKDIR /nextjs
ADD ./ ./

RUN npm install

EXPOSE 8080

CMD node server.js
