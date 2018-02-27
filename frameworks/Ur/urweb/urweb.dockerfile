FROM tfb/urweb-base:latest

CMD ./bench.exe -q -k -t $((2 * ${CPU_COUNT}))
