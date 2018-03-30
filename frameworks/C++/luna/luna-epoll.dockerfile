FROM techempower/luna-base:0.1

CMD /luna/bin/lunabench_epoll 8080 $((2 * $(nproc)))
