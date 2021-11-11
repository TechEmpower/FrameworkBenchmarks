FROM perl:latest

RUN apt-get update -yqq && apt-get install -yqq nginx
RUN cpanm --notest --no-man-page Plack JSON::XS Unix::Processors DBI DBD::mysql
RUN cpanm --notest --no-man-page Cookie::Baker::XS Twiggy::Prefork HTTP::Parser::XS EV AnyEvent::DBI

ADD nginx.conf ./
ADD app.pl ./
ADD app-async.psgi ./

EXPOSE 8080

CMD perl app.pl -a
