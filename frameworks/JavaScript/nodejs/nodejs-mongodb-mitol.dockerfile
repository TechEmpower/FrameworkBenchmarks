FROM mitol:latest

ENV NODE_HANDLER mongoose

CMD ["node", "app-mitol.js"]