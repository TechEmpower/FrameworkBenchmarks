FROM techempower/dart-lang:0.1

COPY ./ ./

RUN pub upgrade

CMD dart server.dart -a 0.0.0.0 -p 8080 -d $MAX_CONCURRENCY -i $(nproc)
