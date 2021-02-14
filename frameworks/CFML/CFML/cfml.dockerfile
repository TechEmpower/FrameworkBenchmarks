FROM ortussolutions/commandbox:3.2.1

COPY ./src/server-lucee.json /app/server.json
COPY ./src/.cfconfig.json /app/.cfconfig.json

RUN box install commandbox-cfconfig --verbose --force

RUN ${BUILD_DIR}/util/warmup-server.sh

RUN export FINALIZE_STARTUP=true;$BUILD_DIR/run.sh;unset FINALIZE_STARTUP

EXPOSE 8080

COPY ./src /app/