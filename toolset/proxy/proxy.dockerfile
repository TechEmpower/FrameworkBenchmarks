FROM debian:buster-slim AS pre-build
RUN apt update
RUN apt upgrade -y
RUN apt install -y g++ curl make tar gzip libfindbin-libs-perl

FROM pre-build AS builder
WORKDIR /build
RUN sh -c "$(curl -sSL https://raw.githubusercontent.com/just-js/just/current/install.sh)"
RUN make -C just install
ENV JUST_HOME=/build/just
ENV JUST_TARGET=/build/just
WORKDIR /app
COPY proxy.js ./
RUN just build --clean --static proxy.js

FROM gcr.io/distroless/static:latest
WORKDIR /app
COPY --from=builder /app/proxy /app/proxy
CMD ["./proxy"]
