FROM techempower/nodejs8:0.1

COPY ./ ./

RUN npm install

ENV NODE_HANDLER mongodb-raw

CMD ["node", "app.js"]
