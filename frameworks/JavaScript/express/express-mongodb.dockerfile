FROM node:14.15.0-slim

COPY ./ ./

RUN npm install

ENV NODE_ENV production

CMD ["node", "mongodb-app.js"]
