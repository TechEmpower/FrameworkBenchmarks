FROM tfb/luna-base:latest

CMD /luna/bin/lunabench_default 8080 $((2 * $CPU_COUNT))
