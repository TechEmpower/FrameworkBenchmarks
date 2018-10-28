FROM node:10.12.0

COPY ./ ./

RUN npm install -g sails
RUN npm install

CMD ["sails", "lift", "--port", "8080"]
