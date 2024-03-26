FROM node:20-slim

COPY ./ ./

RUN npm install

ENV NODE_ENV production
ENV DATABASE mongodb

EXPOSE 8080

CMD ["node", "app.js"]
