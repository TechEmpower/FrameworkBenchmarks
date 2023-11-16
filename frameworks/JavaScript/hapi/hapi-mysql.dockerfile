FROM node:18.12.1-slim

COPY ./ ./

RUN npm install

ENV NODE_HANDLER sequelize

EXPOSE 8080

CMD ["node", "app.js"]
