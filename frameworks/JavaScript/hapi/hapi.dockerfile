FROM node:20.16-slim

COPY ./ ./

RUN npm install

ENV NODE_HANDLER mongoose

CMD ["node", "app.js"]
