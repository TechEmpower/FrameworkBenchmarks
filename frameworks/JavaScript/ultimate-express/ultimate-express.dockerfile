# syntax=docker/dockerfile:1
FROM node:20-slim

WORKDIR /app

COPY --chown=node:node . .

ENV NODE_ENV production

RUN npm install

USER node

EXPOSE 8080

CMD ["node", "clustered.js"]