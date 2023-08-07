###################
# BUILD
###################

FROM node:hydrogen-slim as builder

WORKDIR /app

COPY . .

RUN npm install

RUN npm run check

ENV RUNTIME node

RUN npm run build

###################
# EXTRACTION
###################

FROM node:hydrogen-slim

WORKDIR /app

ENV NODE_ENV production
ENV RUNTIME node

ENV PORT 8080
EXPOSE 8080

COPY --from=builder /app/package.json .
COPY --from=builder /app/package-lock.json .

RUN npm ci --omit dev

COPY --from=builder /app/build ./build
COPY --from=builder /app/fork.js .

CMD ["node", "fork.js"]