FROM node:chakracore

COPY ./ ./

RUN npm install

ENV NODE_ENV production

CMD ["node", "app.js"]
