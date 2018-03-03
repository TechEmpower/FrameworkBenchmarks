FROM tfb/mitol:latest

ENV NODE_HANDLER mysql-raw

CMD ["node", "app-mitol.js"]
