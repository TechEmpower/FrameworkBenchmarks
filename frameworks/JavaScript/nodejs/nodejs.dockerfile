FROM node:21.1.0-slim

ARG TFB_TEST_NAME

COPY ./ ./

RUN npm install

ENV NODE_ENV production
ENV TFB_TEST_NAME=$TFB_TEST_NAME

EXPOSE 8080

CMD ["node", "app.js"]
