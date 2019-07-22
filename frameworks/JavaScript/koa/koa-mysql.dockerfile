FROM node:12.3.1-slim

COPY ./ ./

RUN npm install

ENV NODE_ENV production
ENV NODE_HANDLER sequelize

CMD ["node", "app.js"]
