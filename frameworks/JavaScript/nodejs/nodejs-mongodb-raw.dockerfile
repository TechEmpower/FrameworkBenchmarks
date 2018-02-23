FROM tfb/nodejs8:latest

COPY ./ ./

RUN npm install

ENV NODE_HANDLER mongodb-raw

CMD ["node", "app.js"]
