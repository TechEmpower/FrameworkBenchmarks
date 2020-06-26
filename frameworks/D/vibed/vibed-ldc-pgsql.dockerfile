FROM dlang2/ldc-ubuntu:1.15.0

WORKDIR /dlang/app
COPY . .

RUN apt-get update -yqq && apt-get install -yqq libpq-dev zlib1g-dev

RUN dub build -b release --compiler=ldc2 --config=postgresql

CMD ["/dlang/app/fwb"]
