FROM dlang2/ldc-ubuntu:latest

ADD ./ /archttp
WORKDIR /archttp

RUN apt-get update -yqq && apt-get install -yqq zlib1g-dev

RUN dub build -b release --compiler=ldc2 --verbose

EXPOSE 1111

CMD ["./archttp-server"]
