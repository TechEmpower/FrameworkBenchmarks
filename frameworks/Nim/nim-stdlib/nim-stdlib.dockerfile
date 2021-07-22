FROM nimlang/nim:latest

ADD ./ /nimstdlib

WORKDIR /nimstdlib

RUN apt-get install -y libpq5

RUN nimble c --d:release --threads:on -y src/nimstdlib.nim -o:nimstdlib

EXPOSE 8080

CMD ./nimstdlib
