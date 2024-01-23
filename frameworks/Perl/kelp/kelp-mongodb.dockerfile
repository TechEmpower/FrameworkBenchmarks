FROM perl:5.26

RUN apt-get update -yqq && apt-get install -yqq nginx

WORKDIR /kelp

RUN cpanm --notest --no-man-page \
        JSON JSON::XS IO::Socket::IP IO::Socket::SSL \
        Kelp@0.9071 \
        DBI@1.636 \
        DBD::mysql@4.033 \
        MongoDB@1.8.1 \
        Kelp::Module::JSON::XS@0.502 \
        HTML::Escape@1.10 \
        HTTP::Parser::XS@0.17 \
        Starman@0.4014

ADD ./app.ini /kelp/
ADD ./app.pl /kelp/
ADD ./nginx.conf /kelp/

ENV MONGO=1

EXPOSE 8080

CMD nginx -c /kelp/nginx.conf && \
    plackup -E production -s Starman --workers=$(nproc) -l /tmp/perl-kelp.sock -a ./app.pl
