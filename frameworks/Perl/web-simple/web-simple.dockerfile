FROM tfb/nginx:latest

FROM tfb/perl:latest

COPY --from=0 /nginx /nginx

ENV NGINX_HOME="/nginx"
ENV PATH=/nginx/sbin:${PATH}

WORKDIR /simple

ENV PERL_CARTON_PATH=/simple/local
ENV PERL5LIB=${PERL_CARTON_PATH}/lib/perl5
ENV PATH=${PERL_CARTON_PATH}/bin:${PERL_HOME}/bin:${PATH}

RUN cpanm --notest --no-man-page  \
        Web::Simple@0.033 \
        DBI@1.637 \
        DBD::mysql@4.043 \
        Plack@1.0044 \
        Starman@0.4014 \
        JSON::XS@3.04

ADD ./conf /simple/
ADD ./app.pl /simple/
ADD ./nginx.conf /simple/

CMD nginx -c /simple/nginx.conf && \
    plackup -E production -s Starman --workers=$(nproc) \
    -l /tmp/perl-simple.sock -a /simple/app.pl
