FROM node:10

WORKDIR /usr/src/app

COPY app.js package.json ./

RUN npm install

EXPOSE 8080

CMD ["node", "app.js"]