FROM node:14.15.5-slim

COPY ./ ./

RUN npm install
RUN npm run build

ENV NODE_ENV production
ENV DATABASE_CONFIGURATION_PROFILE postgres
ENV FRAMEWORK fastify

EXPOSE 8080
CMD ["node", "dist/main"]