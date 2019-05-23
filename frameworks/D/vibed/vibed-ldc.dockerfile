FROM dlang2/ldc-ubuntu:1.15.0

WORKDIR /dlang/app
COPY . .

RUN apt update -yqq && apt install -yqq zlib1g-dev

RUN dub build -b release --compiler=ldc2 --verbose

CMD ["/dlang/app/fwb"]
