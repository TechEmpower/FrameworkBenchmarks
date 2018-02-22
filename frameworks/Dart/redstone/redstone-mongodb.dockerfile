FROM dart-lang:latest

COPY ./ ./

RUN pub upgrade

CMD dart server.dart -a 0.0.0.0 -p 8080 -d ${CPU_COUNT} -i ${CPU_COUNT} &
