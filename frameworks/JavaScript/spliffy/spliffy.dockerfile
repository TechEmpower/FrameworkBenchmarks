FROM node:14-buster-slim

RUN apt update && apt install -y libpq-dev g++ make git python node-gyp
COPY package.json /app/
WORKDIR /app
RUN npm install && npm rebuild
COPY . /app

EXPOSE 10420

CMD ["node", "/app/start.js"]
