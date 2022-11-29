FROM node:18.12.1-slim

COPY ./ ./

RUN npm install

ENV NODE_ENV production

EXPOSE 8080

CMD ["node", "mongodb-app.js"]
