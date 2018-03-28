FROM tfb/gcc-4.8:latest

WORKDIR /installs

ENV VERSION=0.2.3
ENV CPPSP_HOME=/installs/cppsp_$VERSION

RUN wget -q http://downloads.sourceforge.net/project/cpollcppsp/CPPSP%200.2%20%28testing%29/cppsp_$VERSION.tar.xz
RUN tar xf cppsp_$VERSION.tar.xz

RUN mv cppsp_rel$VERSION/ $CPPSP_HOME

RUN sed -i 's|CXX := .*|CXX := g++-4.8|g' $CPPSP_HOME/makefile
RUN sed -i 's|-Wall|-w|g' $CPPSP_HOME/makefile

RUN apt install -yqq postgresql-server-dev-9.5
ENV CPLUS_INCLUDE_PATH=/usr/include/postgresql:/usr/include/postgresql/9.5/server:${CPLUS_INCLUDE_PATH}

ADD ./ /cpoll_cppsp
WORKDIR /cpoll_cppsp

RUN make clean && make

WORKDIR $CPPSP_HOME
