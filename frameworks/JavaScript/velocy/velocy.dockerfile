FROM node:20-slim
WORKDIR /usr/app
COPY ./ /usr/app
RUN npm install
EXPOSE 8080
CMD ["node", "app.js"]
