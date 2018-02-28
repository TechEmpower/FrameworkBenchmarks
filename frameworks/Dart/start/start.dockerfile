FROM tfb/dart-lang:latest

COPY ./ ./

RUN pub upgrade

CMD ["./start-servers.sh"]
