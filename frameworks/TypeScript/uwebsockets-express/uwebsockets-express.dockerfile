FROM node:20-slim as base

RUN apt update
RUN apt install -y git

WORKDIR /app

# BUILD

FROM base as builder

COPY . .

RUN npm install

RUN npm run build

# DEPLOY

FROM base

ENV NODE_ENV production

COPY --from=builder /app/package.json .
COPY --from=builder /app/package-lock.json .

RUN npm ci --omit dev

COPY --from=builder /app/build ./build
COPY --from=builder /app/fork.js .

EXPOSE 8080

CMD ["node", "fork.js"]