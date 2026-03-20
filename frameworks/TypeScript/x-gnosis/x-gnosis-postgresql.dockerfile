FROM node:22-slim

EXPOSE 8080

WORKDIR /app

COPY package.json .
RUN npm install --production=false

COPY src/ src/

ENV NODE_ENV=production

CMD ["node", "--import", "tsx", "src/spawn.ts"]
