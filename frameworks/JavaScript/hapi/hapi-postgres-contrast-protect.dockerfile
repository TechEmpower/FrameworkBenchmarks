FROM node:16.13.0-slim

COPY ./ ./

RUN npm install

ENV NODE_HANDLER sequelize-postgres

EXPOSE 8080

# Start Contrast Additions
COPY node-contrast.tgz node-contrast.tgz
COPY contrast_security.yaml /etc/contrast/contrast_security.yaml

ENV CONTRAST__ASSESS__ENABLE=false
ENV CONTRAST__PROTECT__ENABLE=true

run npm install ./node-contrast.tgz
# End Contrast Additions

CMD ["node", "-r", "@contrast/agent", "app.js"]
