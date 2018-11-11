FROM node:10.12.0

COPY ./ ./

RUN npm install

ENV NODE_ENV production
ENV NODE_HANDLER mongoose

CMD ["node", "app.js"]
