FROM dlang2/dmd-ubuntu:2.085.1

WORKDIR /dlang/app
COPY . .

RUN apt-get update -yqq && apt-get install -yqq libpq-dev zlib1g-dev

RUN dub build -b release --config=postgresql

EXPOSE 8080

CMD ["/dlang/app/fwb"]
