FROM techempower/d-lang:0.1

COPY ./ ./

RUN dub upgrade --verbose

RUN dub build -f -b release -v

CMD ["./website"]
