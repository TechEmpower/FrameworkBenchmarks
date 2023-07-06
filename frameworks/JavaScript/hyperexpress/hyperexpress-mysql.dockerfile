# syntax=docker/dockerfile:1
FROM node:18-slim

WORKDIR /app

COPY --chown=node:node . .

ENV NODE_ENV production

ENV DATABASE mysql

RUN npm install

USER node

EXPOSE 8080

CMD ["node", "clustered.js"]