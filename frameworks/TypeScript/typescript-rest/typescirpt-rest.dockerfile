FROM node:10

COPY . .

RUN yarn

CMD ["yarn", "start"]
