FROM dart-nginx:latest

COPY ./ ./

CMD ./setup.sh
