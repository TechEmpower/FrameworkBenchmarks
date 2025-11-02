FROM dlangdockerized/ldc


WORKDIR /app

COPY . .

RUN apt-get update -yqq && apt-get install -yqq libpq-dev zlib1g-dev

RUN dub build -b release --compiler=ldc2

EXPOSE 8080

CMD ["/app/photon-http-benchmark"]

