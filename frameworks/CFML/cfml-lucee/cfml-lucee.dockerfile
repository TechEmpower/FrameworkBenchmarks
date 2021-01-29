FROM ortussolutions/commandbox:latest

COPY ./src/server.json /app/server.json
COPY ./src/.cfconfig.json /app/.cfconfig.json

EXPOSE 8088

RUN box install commandbox-cfconfig --verbose --force --debug

RUN ${BUILD_DIR}/util/warmup-server.sh

RUN export FINALIZE_STARTUP=true;$BUILD_DIR/run.sh;unset FINALIZE_STARTUP

COPY ./src /app/