FROM node:16.14.0

COPY ./ ./

RUN npm install

ENV NODE_ENV production
ENV DATABASE mongo

EXPOSE 8080

CMD ["node", "app.js"]
