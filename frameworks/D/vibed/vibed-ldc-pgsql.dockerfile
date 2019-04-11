FROM dlang2/ldc-ubuntu:1.15.0

WORKDIR /dlang/app
COPY . .

RUN apt update -yqq && apt install -yqq libpq-dev zlib1g-dev

RUN dub build -b release-nobounds --compiler=ldc2 --config=postgresql

CMD ["/dlang/app/fwb"]
