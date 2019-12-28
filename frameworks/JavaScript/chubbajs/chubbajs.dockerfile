FROM node:12.3.1-slim

RUN apt-get update -yqq
RUN apt-get install -yqq git

COPY ./package.json ./package.json

RUN npm install

copy ./server ./server
copy ./.babelrc ./.babelrc

RUN npm run build

CMD ["npm", "run", "start:prod"]
