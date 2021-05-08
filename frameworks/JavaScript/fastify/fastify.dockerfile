FROM node:14.16.1

COPY ./ ./

RUN npm install

ENV NODE_ENV production
ENV DATABASE mongo

EXPOSE 8080

CMD ["node", "app.js"]
