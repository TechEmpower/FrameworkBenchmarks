FROM tfb/nodejs8:latest

COPY ./ ./

RUN npm install

ENV NODE_ENV production

CMD ["node", "mongodb-app.js"]
