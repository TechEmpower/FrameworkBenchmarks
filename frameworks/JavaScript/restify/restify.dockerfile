FROM node:12.3.1-slim

WORKDIR /nextjs
ADD ./ ./

RUN npm install

CMD node server.js
