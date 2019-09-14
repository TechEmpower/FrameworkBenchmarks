FROM node:12.3.1-slim

COPY ./ ./

RUN yarn install

ENV NODE_ENV production

CMD ["yarn", "start:prod"]