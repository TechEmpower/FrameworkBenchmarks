FROM freepascal/fpc:3.2.2-focal-full as builder

RUN apt-get update -yqq
RUN apt-get install -yqq p7zip-full zlib1g-dev

WORKDIR /build
COPY src/ src/
COPY setup_and_build.sh .

RUN /bin/bash -c ./setup_and_build.sh

FROM ubuntu:22.04
COPY --from=builder /build/bin/fpc-x86_64-linux/raw /usr/local/bin/raw

RUN apt-get update && apt-get install -yqq postgresql-client

EXPOSE 8080
CMD ["raw"]


