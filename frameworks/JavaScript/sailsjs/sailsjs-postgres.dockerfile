FROM node:12.3.1-slim

COPY ./ ./

RUN npm install -g sails
RUN npm install

EXPOSE 8080

CMD ["sails", "lift", "--port", "8080"]
