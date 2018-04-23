FROM perl:5.26

WORKDIR /mojo

ADD ./cpanfile* ./

ENV PERL_CARTON_PATH=/kelp/local
ENV PERL5LIB=${PERL_CARTON_PATH}/lib/perl5
ENV PATH=${PERL_CARTON_PATH}/bin:${PERL_HOME}/bin:${PATH}

RUN cpanm --notest --no-man-page \
        Carton JSON JSON::XS IO::Socket::IP IO::Socket::SSL

RUN carton install --cpanfile /mojo/cpanfile

ENV  LIBEV_FLAGS=7

ADD ./app.pl ./

CMD hypnotoad -f /mojo/app.pl
