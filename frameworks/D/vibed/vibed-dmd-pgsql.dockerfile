FROM tfb/d-lang:latest

COPY ./ ./

RUN apt install -yqq libpq-dev zlib1g-dev

RUN dub upgrade --verbose
RUN dub build -b release --combined --config=postgresql

CMD ["./fwb"]
