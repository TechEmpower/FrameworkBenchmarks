FROM node:16

COPY ./ ./

ENV NODE_ENV development

RUN npm install

ENV DATABASE_CONFIGURATION_PROFILE postgres
ENV FRAMEWORK express

EXPOSE 8080
CMD ["npm", "run", "start"]