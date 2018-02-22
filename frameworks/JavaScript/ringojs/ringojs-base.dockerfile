FROM java:latest

ENV NODE_ENV=production

RUN curl -sL https://deb.nodesource.com/setup_8.x | bash -
RUN apt install -y nodejs

ENV RINGO_VERSION 1.1.0

RUN wget -O https://github.com/ringo/ringojs/releases/download/v$VERSION/ringojs-${RINGO_VERSION}.tar.gz
RUN tar xvf ringojs-${RINGO_VERSION}.tar.gz

ENV PATH="ringojs-${RINGO_VERSION}/bin:${PATH}"
