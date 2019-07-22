FROM node:10-alpine

WORKDIR /usr/src/app

COPY app.js package.json ./

RUN npm install
RUN npm install uNetworking/uWebSockets.js#v15.11.0

CMD ["node", "app.js"]