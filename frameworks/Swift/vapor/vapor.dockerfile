FROM swift:5.1 as builder

RUN apt-get -y update
RUN apt-get -y install libssl-dev zlib1g-dev
COPY ./app /app
WORKDIR /app
RUN swift build -c release

FROM swift:5.1-slim as runtime
COPY --from=builder /app /app
WORKDIR /app
CMD .build/release/app serve -e production -b 0.0.0.0:8080 --log notice
