FROM node:12.3.1-slim

COPY ./ ./

RUN yarn install

ENV NODE_ENV production
ENV DATABASE_CONFIGURATION_PROFILE=postgres

CMD ["yarn", "start:prod"]