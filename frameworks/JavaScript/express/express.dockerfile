FROM node:10.3.0

COPY ./ ./

RUN npm install

ENV NODE_ENV production

CMD ["node", "app.js"]
