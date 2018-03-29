FROM tfb/urweb-base:latest

RUN urweb -dbms mysql -db "dbname=hello_world user=benchmarkdbuser password=benchmarkdbpass host=TFB-database" bench

CMD ./bench.exe -q -k -t $((2 * $(nproc)))
