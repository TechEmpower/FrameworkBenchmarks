FROM techempower/base:0.1

RUN mkdir /crystal-lang
WORKDIR /crystal-lang
RUN curl -sL https://github.com/crystal-lang/crystal/releases/download/v0.24.1/crystal-0.24.1-2-linux-x86_64.tar.gz | tar xz
ENV CRYSTAL_HOME=/crystal-lang/crystal-0.24.1
ENV PATH="${CRYSTAL_HOME}/bin:${PATH}"
