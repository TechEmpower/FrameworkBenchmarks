FROM node:9.10.1

COPY ./ ./

RUN npm install

ENV NODE_ENV production

CMD ["node", "mysql-app.js"]
