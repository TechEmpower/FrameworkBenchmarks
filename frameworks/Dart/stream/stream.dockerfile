FROM tfb/dart-nginx:latest

COPY ./ ./

CMD ["./stream-server.sh"]
