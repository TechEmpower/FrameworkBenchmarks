FROM nimlang/nim:latest

ADD ./ /prologue
WORKDIR /prologue
RUN nim install -y
RUN nim c -r -d:release --threads:on
