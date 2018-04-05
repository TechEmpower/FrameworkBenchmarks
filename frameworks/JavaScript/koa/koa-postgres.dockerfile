FROM techempower/nodejs8:0.1

COPY ./ ./

RUN npm install

ENV NODE_ENV production
ENV NODE_HANDLER sequelize-postgres

CMD ["node", "app.js"]
