FROM techempower/urweb-base:0.1

RUN urweb -db "dbname=hello_world user=benchmarkdbuser password=benchmarkdbpass host=TFB-database" bench

CMD ./bench.exe -q -k -t $((2 * ${CPU_COUNT}))
