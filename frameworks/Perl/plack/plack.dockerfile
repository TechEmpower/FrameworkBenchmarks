FROM perl:5.26

RUN apt update -yqq && apt install -yqq nginx

WORKDIR /plack

RUN cpanm --notest --no-man-page \
        JSON JSON::XS IO::Socket::IP IO::Socket::SSL \
        JSON::XS@3.01 \
        HTTP::Parser::XS@0.16 \
        Plack@1.0030 \
        DBI@1.631 \
        DBD::mysql@4.033 \
        Starlet@0.24

ADD ./app.pid /plack/
ADD ./app.psgi /plack/
add ./nginx.conf /plack/

CMD nginx -c /plack/nginx.conf && \
    start_server --backlog=16384 --pid-file=/plack/app.pid --path=/tmp/perl-plack.sock -- plackup \
    -E production -s Starlet --max-keepalive-reqs 1000 --max-reqs-per-child 50000 \
    --min-reqs-per-child 40000 --max-workers=$(nproc) -a /plack/app.psgi
