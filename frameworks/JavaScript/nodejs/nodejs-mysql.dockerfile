FROM node:10.12.0

COPY ./ ./

RUN npm install

ENV NODE_HANDLER sequelize 

CMD ["node", "app.js"]
