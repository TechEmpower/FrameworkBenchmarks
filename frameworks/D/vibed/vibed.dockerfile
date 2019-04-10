FROM dlang2/dmd-ubuntu:2.085.1

WORKDIR /dlang/app
COPY . .

RUN apt update -yqq && apt install -yqq libpq-dev zlib1g-dev

RUN dub build -b release-nobounds --verbose

CMD ["/dlang/app/fwb"]
