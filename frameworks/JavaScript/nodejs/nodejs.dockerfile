FROM tfb/nodejs8:latest

COPY ./ ./

RUN npm install

ENV NODE_HANDLER mysql-raw

CMD ["node", "app.js"]
