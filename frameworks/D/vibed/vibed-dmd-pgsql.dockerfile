FROM dlang2/dmd-ubuntu:2.085.1

WORKDIR /dlang/app
COPY . .

RUN apt update -yqq && apt install -yqq libpq-dev zlib1g-dev

RUN dub build -b release-nobounds --config=postgresql

CMD ["/dlang/app/fwb"]
