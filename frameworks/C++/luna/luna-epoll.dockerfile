FROM tfb/luna-base:latest

CMD /luna/bin/lunabench_epoll 8080 $((2 * $(nproc)))
