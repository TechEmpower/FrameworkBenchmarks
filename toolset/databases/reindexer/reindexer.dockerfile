FROM reindexer/reindexer:latest

ADD . /src
RUN reindexer_tool --dsn builtin:///db/hello_world --filename /src/create.rxdump
RUN for I in $(seq 1 10000); do echo "\\upsert world {\"id\":$I,\"randomNumber\":$(( ( RANDOM % 10000 )  + 1 ))}" ; done | reindexer_tool --dsn builtin:///db/hello_world

CMD reindexer_server --db /db --httpaddr 0:9088 --rpcaddr 0:6534 --webroot /usr/local/share/reindexer/web --rpclog "" --httplog "" --corelog ""
