FROM node:chakracore

COPY ./ ./

RUN npm install

ENV NODE_HANDLER mysql-raw

CMD ["node", "app.js"]
