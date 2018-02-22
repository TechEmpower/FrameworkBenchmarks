FROM tfb/nodejs8:latest

COPY ./ ./

RUN npm install

ENV NODE_HANDLER sequelize

CMD ["node", "app.js"]
