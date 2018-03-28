FROM techempower/luna-base:0.1

CMD /luna/bin/lunabench_default 8080 $((2 * $CPU_COUNT))
