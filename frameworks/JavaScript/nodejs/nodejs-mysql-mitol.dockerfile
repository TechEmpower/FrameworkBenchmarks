FROM tfb/mitol:latest

ENV NODE_HANDLER sequelize

CMD ["node", "app-mitol.js"]
