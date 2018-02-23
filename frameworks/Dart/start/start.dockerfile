FROM tfb/dart:latest

COPY ./ ./

CMD ["./setup.sh"]
