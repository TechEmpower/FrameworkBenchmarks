FROM java:latest

COPY ./ ./

ENV RINGOJS_VERSION="1.1.0"
ENV RINGOJS_HOME=/ringojs-$RINGOJS_VERSION

RUN wget https://github.com/ringo/ringojs/releases/download/v$RINGOJS_VERSION/ringojs-$RINGOJS_VERSION.tar.gz && \
    tar xvf ringojs-$RINGOJS_VERSION.tar.gz

ENV PATH=${RINGOJS_HOME}/bin:${PATH}
