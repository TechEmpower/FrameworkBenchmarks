FROM perl:latest

RUN apt-get update -yqq && apt-get install -yqq nginx
RUN cpanm --notest --no-man-page Plack JSON::XS Unix::Processors DBI DBD::mysql
RUN cpanm --notest --no-man-page Gazelle Cookie::Baker::XS

ADD nginx.conf ./
ADD app.pl ./
ADD app.psgi ./

EXPOSE 8080

CMD perl app.pl
