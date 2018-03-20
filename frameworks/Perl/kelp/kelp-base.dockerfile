FROM tfb/nginx:latest

FROM tfb/perl:latest

COPY --from=0 /nginx /nginx

ENV NGINX_HOME="/nginx"
ENV PATH=/nginx/sbin:${PATH}

WORKDIR /kelp

ENV PERL_CARTON_PATH=/kelp/local
ENV PERL5LIB=${PERL_CARTON_PATH}/lib/perl5
ENV PATH=${PERL_CARTON_PATH}/bin:${PERL_HOME}/bin:${PATH}

RUN cpanm --notest --no-man-page \
        Kelp@0.9071 \
        DBI@1.636 \
        DBD::mysql@4.033 \
        MongoDB@1.4.2 \
        Kelp::Module::JSON::XS@0.502 \
        HTML::Escape@1.10 \
        HTTP::Parser::XS@0.17 \
        Starman@0.4014

ADD ./app.ini /kelp/
ADD ./app.pl /kelp/
add ./nginx.conf /kelp/
