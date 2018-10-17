FROM node:10.12.0

WORKDIR /nextjs
ADD ./ ./

RUN npm install

CMD node server.js
