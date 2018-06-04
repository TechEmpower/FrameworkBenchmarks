FROM node:10.3.0

WORKDIR /nextjs
ADD ./ ./

RUN npm install

CMD node server.js
