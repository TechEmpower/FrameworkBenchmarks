FROM nodejs8:latest

COPY ./ ./

RUN npm install

ENV NODE_HANDLER mongoose

CMD ["node", "app.js"]
