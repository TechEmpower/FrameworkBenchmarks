FROM tfb/kelp-base:latest

ENV MONGO=1

RUN sleep 10

CMD nginx -c /kelp/nginx.conf && \
    plackup -E production -s Starman --workers=${CPU_COUNT} -l /tmp/perl-kelp.sock -a ./app.pl
