FROM dlanguage/ldc:1.7.0

WORKDIR /hunt
COPY config config
COPY source source
COPY dub.json dub.json
COPY dub.selections.json dub.selections.json

RUN dub upgrade --verbose
RUN dub build -f -b release -v

CMD ["./website"]
