FROM node:20.12.2-alpine

COPY ./ ./

RUN npm install

ENV NODE_ENV production
ENV DATABASE mysql

EXPOSE 8080

CMD ["node", "app.js"]
