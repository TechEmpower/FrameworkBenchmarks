FROM node:12.3.1-slim

COPY ./ ./

RUN npm install

ENV NODE_ENV production

CMD ["node", "graphql-postgres-app.js"]
