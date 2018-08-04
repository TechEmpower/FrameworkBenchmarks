FROM reindexer/reindexer

ADD . /src
RUN reindexer_tool --dsn builtin:///db/techempower --filename /src/create.rxdump
RUN for I in $(seq 1 10000); do echo "\\upsert world {\"id\":$I,\"randomNumber\":$(( ( RANDOM % 10000 )  + 1 ))}" ; done | reindexer_tool --dsn builtin:///db/techempower

CMD reindexer_server --db /db --httpaddr 0:9088 --rpcaddr 0:6534 --webroot /usr/local/share/reindexer/web --rpclog "" --httplog "" --corelog ""
