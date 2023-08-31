FROM node:20-slim

COPY ./ ./

RUN npm install

EXPOSE 8080

CMD ["node", "app.js"]
