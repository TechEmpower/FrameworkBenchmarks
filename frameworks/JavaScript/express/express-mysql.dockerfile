FROM nodejs8:latest

COPY ./ ./

RUN npm install

ENV NODE_ENV production

CMD ["node", "mysql-app.js"]
