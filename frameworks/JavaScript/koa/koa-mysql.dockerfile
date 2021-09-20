FROM node:16.9.1

COPY ./ ./

RUN npm install

ENV NODE_ENV production
ENV NODE_HANDLER sequelize

EXPOSE 8080

CMD ["node", "app.js"]
