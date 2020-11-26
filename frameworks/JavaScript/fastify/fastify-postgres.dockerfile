FROM node:14.6.0-slim

COPY ./ ./

RUN npm install

ENV NODE_ENV production
ENV DATABASE postgres

CMD ["node", "app.js"]
