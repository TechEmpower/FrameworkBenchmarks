FROM node:12.3.1-slim

ENV NODE_ENV production

RUN apt-get update -yqq
RUN apt-get install -yqq git

COPY ./package.json ./package.json

RUN npm install

COPY ./server ./server
COPY ./.babelrc ./.babelrc

RUN npm run build

CMD ["npm", "run", "start:prod"]
