FROM techempower/nginx:0.1

FROM techempower/perl:0.1

COPY --from=0 /nginx /nginx

ENV NGINX_HOME="/nginx"
ENV PATH=/nginx/sbin:${PATH}

WORKDIR /plack

ENV PERL_CARTON_PATH=/plack/local
ENV PERL5LIB=${PERL_CARTON_PATH}/lib/perl5
ENV PATH=${PERL_CARTON_PATH}/bin:${PERL_HOME}/bin:${PATH}

RUN cpanm --notest --no-man-page \
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
    --min-reqs-per-child 40000 --max-workers=${CPU_COUNT} -a /plack/app.psgi
