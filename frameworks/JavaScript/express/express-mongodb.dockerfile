FROM node:10.12.0

COPY ./ ./

RUN npm install

ENV NODE_ENV production

CMD ["node", "mongodb-app.js"]
