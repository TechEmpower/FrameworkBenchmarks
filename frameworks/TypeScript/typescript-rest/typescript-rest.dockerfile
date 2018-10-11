FROM node:10

WORKDIR /home
COPY . .

ENV PORT 8080

RUN rm -rf node_modules/
RUN yarn install --pure-lockfile

CMD ["yarn", "start"]
