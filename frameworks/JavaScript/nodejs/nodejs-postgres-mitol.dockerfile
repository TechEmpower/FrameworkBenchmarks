FROM mitol:latest

ENV NODE_HANDLER sequelize-postgres

CMD ["node", "app-mitol.js"]