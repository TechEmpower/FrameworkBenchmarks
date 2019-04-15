FROM node:10.12.0

ARG TFB_TEST_NAME

COPY ./ ./

RUN npm install

ENV TFB_TEST_NAME=$TFB_TEST_NAME

CMD ["node", "app.js"]
