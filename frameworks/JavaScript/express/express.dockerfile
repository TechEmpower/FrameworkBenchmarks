FROM techempower/nodejs8:0.1

COPY ./ ./

RUN npm install

ENV NODE_ENV production

CMD ["node", "app.js"]
