FROM ortussolutions/commandbox:latest

COPY ./src /app/

RUN ${BUILD_DIR}/util/warmup-server.sh

RUN export FINALIZE_STARTUP=true;$BUILD_DIR/run.sh;unset FINALIZE_STARTUP
