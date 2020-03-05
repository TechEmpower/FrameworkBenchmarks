FROM node:12.16.1-slim

COPY ./ ./

RUN npm install
RUN npm run build

ENV NODE_ENV production
ENV DATABASE_CONFIGURATION_PROFILE mongodb
ENV FRAMEWORK express

EXPOSE 8080
CMD ["node", "dist/main"]