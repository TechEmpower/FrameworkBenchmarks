FROM techempower/kelp-base:0.1

CMD nginx -c /kelp/nginx.conf && \
    plackup -E production -s Starman --workers=${CPU_COUNT} -l /tmp/perl-kelp.sock -a ./app.pl
