FROM perl:latest

RUN apt update -yqq && apt install -yqq nginx
RUN cpanm --notest --no-man-page Plack JSON::XS Unix::Processors DBI DBD::mysql
RUN cpanm --notest --no-man-page Cookie::Baker::XS Twiggy::Prefork HTTP::Parser::XS EV AnyEvent::DBI

ADD nginx.conf ./
ADD app.pl ./
ADD app-async.psgi ./

CMD perl app.pl -a
