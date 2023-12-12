# docker build --progress=plain --build-arg CXXFLAGS="-Wall" -t just-boost -f just-boost.dockerfile .
# docker run --rm --name just-boost -p 8000:8000 -d just-boost
# docker container stop just-boost
FROM alpine:3.19

ARG APP=just-boost
ARG CXXFLAGS=-O3

ENV LANG=C.UTF-8 LC_ALL=C.UTF-8
ENV BCPP_PG_CONN_STR="postgres://benchmarkdbuser:benchmarkdbpass@tfb-database/hello_world"
#ENV BCPP_N_THREADS=0 # default 0 : number of cores

WORKDIR /usr/src/${APP}

RUN apk add --no-cache build-base boost-dev libpq-dev
COPY *.cpp ./
RUN g++ ${CXXFLAGS} -std=c++20 \
        -I$(pg_config --includedir) \
        -o main main.cpp \
        -L$(pg_config --libdir) -lpq \
        && rm *.cpp

EXPOSE 8000

CMD ["./main"]
