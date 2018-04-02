FROM techempower/kelp-base:0.1

ENV MONGO=1

RUN sleep 10

CMD nginx -c /kelp/nginx.conf && \
    plackup -E production -s Starman --workers=$(nproc) -l /tmp/perl-kelp.sock -a ./app.pl
