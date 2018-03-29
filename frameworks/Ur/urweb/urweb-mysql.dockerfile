FROM techempower/urweb-base:0.1

RUN urweb -dbms mysql -db "dbname=hello_world user=benchmarkdbuser password=benchmarkdbpass host=tfb-database" bench

CMD ./bench.exe -q -k -t $((2 * ${CPU_COUNT}))
