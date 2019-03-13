FROM node:10.12.0

ARG TFB_TEST_NAME

COPY ./ ./

RUN npm install

CMD ["node", "app.js"]
