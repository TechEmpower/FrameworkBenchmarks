FROM node:22-slim

ENV NEXT_TELEMETRY_DISABLED="1"
ENV DATABASE_URL="postgres://benchmarkdbuser:benchmarkdbpass@tfb-database/hello_world"

EXPOSE 3000

WORKDIR /nextjs

COPY package.json package-lock.json ./
RUN npm ci

COPY ./ ./
RUN npm run build \
 && cp -r public .next/standalone/ \
 && cp -r .next/static .next/standalone/.next/

ENV NODE_ENV="production"

CMD ["node", ".next/standalone/server.js"]
