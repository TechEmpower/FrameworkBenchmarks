FROM nodejs8:latest

COPY ./ ./

RUN npm install -g sails
RUN npm install

CMD ["sails", "lift", "--port", "8080"]
