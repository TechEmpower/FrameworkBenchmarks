FROM node:10.3.0

COPY ./ ./

RUN npm install

ENV NODE_ENV production
ENV NODE_HANDLER sequelize

CMD ["node", "app.js"]
