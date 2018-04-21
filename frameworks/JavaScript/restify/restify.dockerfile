FROM node:9.10.1

WORKDIR /nextjs
ADD ./ ./

RUN npm install

CMD node server.js
