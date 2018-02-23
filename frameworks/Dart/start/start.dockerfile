FROM tfb/dart-lang:latest

COPY ./ ./

CMD ["./start-server.sh"]
