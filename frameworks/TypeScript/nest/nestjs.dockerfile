FROM node:10.3.0

COPY ./ ./

RUN yarn install

# ENV NODE_ENV production

CMD ["yarn", "start"]