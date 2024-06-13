FROM perl:5.40

ARG TFB_TEST_NAME
ARG TFB_TEST_DATABASE

RUN apt-get update -yqq && apt-get install -yqq nginx

WORKDIR /kelp

RUN cpanm --notest --no-man-page \
	Kelp::Module::Template::Toolkit@0.301 \
	Kelp@2.00 \
	DBI@1.643 \
	DBD::MariaDB@1.23 \
	MongoDB@2.2.2 \
	Cpanel::JSON::XS@4.38 \
	Gazelle@0.49 \
	Starman@0.4017 \
	Starlet@0.31 \
	Twiggy::Prefork@0.08 \
	Net::Server::SS::PreFork@0.05

ADD ./ /kelp/

ENV TEST_NAME=$TFB_TEST_NAME
ENV DATABASE=$TFB_TEST_DATABASE
ENV MAX_REQS=100000
ENV SOCKET_FILE=/tmp/perl-kelp.sock

EXPOSE 8080

CMD nginx -c /kelp/nginx.conf && ./run.pl

