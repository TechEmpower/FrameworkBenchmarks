FROM ortussolutions/commandbox:3.2.1

ENV cfconfig_adminPassword=password

COPY ./src/server-lucee.json /app/server.json
COPY ./src/.cfconfig.json /app/.cfconfig.json
COPY ./src/box.json /app/box.json

RUN box install --verbose --force

RUN ${BUILD_DIR}/util/warmup-server.sh

RUN box cfconfig import from=/app/.cfconfig.json toFormat=luceeWeb

RUN export FINALIZE_STARTUP=true;$BUILD_DIR/run.sh;unset FINALIZE_STARTUP

HEALTHCHECK NONE

EXPOSE 8080

COPY ./src /app/