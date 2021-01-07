FROM node:12.3.1-slim

COPY ./ ./

RUN npm install

ENV NODE_ENV production
ENV DATABASE mysql

EXPOSE 8080

CMD ["node", "app.js"]
