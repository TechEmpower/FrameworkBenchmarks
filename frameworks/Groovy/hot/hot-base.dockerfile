FROM techempower/java8:0.1

ADD ./ /hot
WORKDIR /hot
ENV HOT_VERSION=0.9.2-SNAPSHOT
RUN curl -sL https://github.com/dsolimando/Hot/releases/download/${HOT_VERSION}/hot-${HOT_VERSION}.tar.gz | tar xz
ENV HOT_HOME=/hot/hot-${HOT_VERSION}
ENV PATH="${HOT_HOME}:${PATH}"
