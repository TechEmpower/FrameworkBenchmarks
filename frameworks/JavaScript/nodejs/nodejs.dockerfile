FROM node:12.3.1-slim

ARG TFB_TEST_NAME

COPY ./ ./

RUN npm install

ENV TFB_TEST_NAME=$TFB_TEST_NAME

CMD ["node", "app.js"]
