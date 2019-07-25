FROM node:10-alpine

WORKDIR /usr/src/app

COPY app.js package.json ./

RUN npm install

CMD ["node", "app.js"]