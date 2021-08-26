FROM node:14.17.5

COPY ./ ./

RUN npm install

ENV NODE_ENV production
ENV DATABASE mysql

EXPOSE 8080

CMD ["node", "app.js"]
