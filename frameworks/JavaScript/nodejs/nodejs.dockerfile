FROM node:14.17.1-slim

ARG TFB_TEST_NAME

COPY ./ ./

RUN npm install

ENV TFB_TEST_NAME=$TFB_TEST_NAME

EXPOSE 8080

CMD ["node", "app.js"]
