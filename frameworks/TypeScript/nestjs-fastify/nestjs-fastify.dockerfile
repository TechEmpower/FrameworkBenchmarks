FROM node:12.16.1

WORKDIR /app

COPY src/ ./src
COPY views/ views/
COPY package*.json ./
COPY ormconfig.js ./
COPY tsconfig*.json ./

RUN npm ci --silent
RUN npm run build

EXPOSE 8080
CMD [ "node", "dist/main" ]
