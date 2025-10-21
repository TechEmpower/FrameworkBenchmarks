# syntax=docker/dockerfile:1
FROM node:25-slim

WORKDIR /app

COPY --chown=node:node . .

ENV NODE_ENV production

RUN npm install ci

USER node

EXPOSE 8080

CMD ["node", "clustered.js"]
