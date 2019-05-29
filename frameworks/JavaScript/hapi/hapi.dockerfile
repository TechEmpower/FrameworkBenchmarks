FROM node:12.3.1-slim

COPY ./ ./

RUN npm install

ENV NODE_HANDLER mongoose

CMD ["node", "app.js"]
