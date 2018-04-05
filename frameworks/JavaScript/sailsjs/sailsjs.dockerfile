FROM techempower/nodejs8:0.1

COPY ./ ./

RUN npm install -g sails
RUN npm install

CMD ["sails", "lift", "--port", "8080"]
