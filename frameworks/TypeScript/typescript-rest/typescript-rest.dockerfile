FROM node:10

COPY . .

ENV PORT 8080

RUN rm -rf node_modules/
RUN yarn

CMD ["yarn", "start"]
