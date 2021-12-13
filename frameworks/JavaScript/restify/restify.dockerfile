FROM node:16.9.1-slim

WORKDIR /nextjs
ADD ./ ./

RUN npm install

EXPOSE 8080

CMD node server.js
