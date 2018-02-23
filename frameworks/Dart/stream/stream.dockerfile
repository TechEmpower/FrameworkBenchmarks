FROM tfb/dart-nginx:latest

COPY ./ ./

CMD ./setup.sh
