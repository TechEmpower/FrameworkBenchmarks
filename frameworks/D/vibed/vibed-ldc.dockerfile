FROM dlang2/ldc-ubuntu:1.24.0

WORKDIR /dlang/app
COPY . .

RUN apt-get update -yqq && apt-get install -yqq zlib1g-dev

RUN dub build -b release --compiler=ldc2 --verbose

EXPOSE 8080

CMD ["/dlang/app/fwb"]
