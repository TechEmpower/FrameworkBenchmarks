FROM tfb/java8:latest

COPY ./ ./

ENV RINGOJS_VERSION="1.1.0"
ENV RINGOJS_HOME=/ringojs-$RINGOJS_VERSION

RUN wget -q https://github.com/ringo/ringojs/releases/download/v$RINGOJS_VERSION/ringojs-$RINGOJS_VERSION.tar.gz && \
    tar xf ringojs-$RINGOJS_VERSION.tar.gz

ENV PATH=${RINGOJS_HOME}/bin:${PATH}
