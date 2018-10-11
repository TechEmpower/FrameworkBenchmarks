FROM node:10

COPY . .

ENV PORT 8080

RUN yarn

CMD ["yarn", "start"]
