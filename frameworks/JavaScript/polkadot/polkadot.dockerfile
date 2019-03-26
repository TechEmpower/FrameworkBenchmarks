FROM node:11.6.0

COPY ./ ./

RUN npm install

ENV NODE_ENV production

CMD ["node", "app.js"]
