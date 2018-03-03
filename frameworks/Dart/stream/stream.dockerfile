FROM tfb/dart-lang:latest

COPY ./ ./

RUN pub upgrade

RUN chmod a+rwx start-servers.sh

CMD ["./start-servers.sh"]
