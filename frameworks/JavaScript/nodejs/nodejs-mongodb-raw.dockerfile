FROM node:10.3.0

COPY ./ ./

RUN npm install

ENV NODE_HANDLER mongodb-raw

CMD ["node", "app.js"]
