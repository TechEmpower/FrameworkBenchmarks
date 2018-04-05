FROM node:9.10.1

COPY ./ ./

RUN npm install

ENV NODE_HANDLER sequelize

CMD ["node", "app.js"]
