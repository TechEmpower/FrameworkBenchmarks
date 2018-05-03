FROM dlanguage/ldc:1.7.0

ADD ./ /vibed
WORKDIR /vibed

RUN apt update -yqq
RUN apt install -yqq libpq-dev zlib1g-dev > /dev/null

RUN dub upgrade --verbose
RUN dub build -b release --combined

CMD ["./fwb"]
