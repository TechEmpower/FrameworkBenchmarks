FROM node:10.7.0

COPY ./ ./

RUN npm install

ENV NODE_ENV production
ENV NODE_HANDLER sequelize

CMD ["node", "app.js"]
