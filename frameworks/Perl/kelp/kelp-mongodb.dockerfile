FROM perl:5.40

RUN apt-get update -yqq && apt-get install -yqq nginx

WORKDIR /kelp

RUN cpanm --notest --no-man-page \
        Kelp::Module::Template::Toolkit@0.301 \
        Kelp@2.00 \
        MongoDB@2.2.2 \
        Cpanel::JSON::XS@4.38 \
        Gazelle@0.49

ADD ./ /kelp/

ENV MONGO=1

EXPOSE 8080

CMD nginx -c /kelp/nginx.conf && \
    start_server --path /tmp/perl-kelp.sock --backlog 16384 -- plackup -E production -s Gazelle --max-workers=$(nproc) --max-reqs-per-child=10000 -a ./app.psgi

