FROM node:14-buster-slim

RUN apt update && apt install -y libpq-dev g++ make git python
COPY package.json /app/
WORKDIR /app
RUN npm install
COPY . /app

EXPOSE 1420

CMD ["node", "/app/start.js"]
