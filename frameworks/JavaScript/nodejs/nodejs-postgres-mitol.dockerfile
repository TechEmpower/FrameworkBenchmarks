FROM tfb/mitol:latest

ENV NODE_HANDLER sequelize-postgres

CMD ["node", "app-mitol.js"]