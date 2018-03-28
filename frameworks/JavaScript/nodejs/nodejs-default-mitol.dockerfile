FROM techempower/mitol:0.1

ENV NODE_HANDLER mysql-raw

CMD ["node", "app-mitol.js"]
