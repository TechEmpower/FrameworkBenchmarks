FROM node:14-buster-slim

RUN apt update && apt install -y libpq-dev g++ make git python
COPY . /app
WORKDIR /app
RUN npm install

ENV DB_HOST=tfb-database
ENV DATABASE=postgres

EXPOSE 1420

CMD ["node", "/app/start.js"]
