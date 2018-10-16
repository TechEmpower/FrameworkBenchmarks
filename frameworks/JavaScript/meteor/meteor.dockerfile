FROM node:10.12.0

RUN curl https://install.meteor.com | sh

RUN meteor --allow-superuser create /meteor-tfb

WORKDIR meteor-tfb

COPY ./server ./server
COPY package.json package.json

RUN npm install

CMD npm start
