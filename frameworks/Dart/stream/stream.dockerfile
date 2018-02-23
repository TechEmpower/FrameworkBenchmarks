FROM tfb/dart-lang:latest

COPY ./ ./

RUN pub upgrade

CMD ["./stream-server.sh"]
