FROM tfb/perl:latest

WORKDIR /mojo

ENV PERL_CARTON_PATH=/kelp/local
ENV PERL5LIB=${PERL_CARTON_PATH}/lib/perl5
ENV PATH=${PERL_CARTON_PATH}/bin:${PERL_HOME}/bin:${PATH}

ADD ./cpanfile* ./

RUN carton install --cpanfile /mojo/cpanfile

ENV  LIBEV_FLAGS=7

ADD ./app.pl ./

CMD hypnotoad -f /mojo/app.pl
