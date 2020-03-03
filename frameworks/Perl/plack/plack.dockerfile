FROM perl:latest

RUN apt update -yqq && apt install -yqq nginx
RUN cpanm --notest --no-man-page Plack JSON::XS Unix::Processors DBI DBD::mysql
RUN cpanm --notest --no-man-page Gazelle Cookie::Baker::XS

ADD nginx.conf ./
ADD app.pl ./
ADD app.psgi ./

CMD perl app.pl