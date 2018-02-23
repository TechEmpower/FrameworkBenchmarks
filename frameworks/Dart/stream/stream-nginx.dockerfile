FROM tfb/dart-nginx:latest

COPY ./ ./

CMD ["./stream-nginx.sh"]
