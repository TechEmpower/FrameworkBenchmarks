FROM techempower/nginx:0.1

FROM techempower/perl:0.1

COPY --from=0 /nginx /nginx

ENV NGINX_HOME="/nginx"
ENV PATH=/nginx/sbin:${PATH}

ADD ./ /dancer

WORKDIR /dancer

ENV PERL_CARTON_PATH=/dancer/local
ENV PERL5LIB=${PERL_CARTON_PATH}/lib/perl5
ENV PATH=${PERL_CARTON_PATH}/bin:${PERL_HOME}/bin:${PATH}

RUN cpanm --notest --no-man-page \
      Dancer@1.3134 \
      Dancer::Plugin::Database@2.10 \
      DBI@1.633 \
      DBD::mysql@4.033 \
      JSON::XS@3.01 \
      Plack@1.0034 \
      Starman@0.4011

CMD nginx -c /dancer/nginx.conf && \
    plackup -E production -s Starman --workers=${CPU_COUNT} -l /tmp/perl-dancer.sock -a ./app.pl
