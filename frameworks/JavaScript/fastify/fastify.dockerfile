FROM node:14.17.0

COPY ./ ./

RUN npm install

ENV NODE_ENV production
ENV DATABASE mongo

EXPOSE 8080

CMD ["node", "app.js"]
