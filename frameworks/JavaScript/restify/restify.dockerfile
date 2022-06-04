FROM node:16.14.2-slim

WORKDIR /nextjs
ADD ./ ./

RUN npm install

EXPOSE 8080

CMD node server.js
