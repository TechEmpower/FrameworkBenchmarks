FROM node:20.16-slim

COPY ./ ./

RUN npm install
RUN npm run build

ENV NODE_ENV production

EXPOSE 8080
CMD node dist/main.js
