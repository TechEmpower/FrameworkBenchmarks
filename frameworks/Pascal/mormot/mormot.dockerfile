FROM freepascal/fpc:3.2.2-focal-full as builder

RUN apt-get update -yqq
RUN apt-get install -yqq zlib1g-dev

WORKDIR /build
COPY src/raw.* src/
COPY setup_and_build.sh .

RUN /bin/bash -c ./setup_and_build.sh

FROM ubuntu:24.04

ARG TFB_TEST_NAME

COPY --from=builder /build/bin/fpc-x86_64-linux/raw /usr/local/bin/raw
COPY --from=builder /build/libpq.so.5.16 /usr/lib/x86_64-linux-gnu/libpq.so.5

ENV TFB_TEST_NAME=$TFB_TEST_NAME

EXPOSE 8080
CMD ["raw"]


