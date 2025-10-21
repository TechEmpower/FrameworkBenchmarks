FROM dlangdockerized/ldc


WORKDIR /app

COPY . .

RUN dub build -b release --compiler=ldc2

EXPOSE 8080

CMD ["/app/photon-http-benchmark"]

