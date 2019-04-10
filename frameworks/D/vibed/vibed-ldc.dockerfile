FROM dlang2/ldc-ubuntu:latest

WORKDIR /dlang/app
COPY . .

RUN apt update -yqq && apt install -yqq zlib1g-dev

RUN dub build -b release-nobounds --compiler=ldc2 --verbose

CMD ["/dlang/app/fwb"]
