FROM node:lts

COPY ./ ./

RUN yarn install

ENV NODE_ENV production

CMD ["yarn", "start:prod"]