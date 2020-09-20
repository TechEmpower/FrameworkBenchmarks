FROM ubuntu:20.04

RUN apt-get update -qq && \
    apt-get install -yqq locales wget build-essential

RUN locale-gen en_US.UTF-8
ENV LANG en_US.UTF-8
ENV LANGUAGE en_US:en
ENV LC_ALL en_US.UTF-8
ENV DEBIAN_FRONTEND noninteractive

ENV TROOT /cutelyst-benchmark-app
ENV CUTELYST_APP ${TROOT}/build/libcutelyst_benchmarks.so

COPY src ${TROOT}/

COPY build.sh .
RUN ./build.sh

COPY config/config.ini /cutelyst.ini
COPY config/config_socket.ini /cutelyst_socket.ini
COPY nginx.conf /nginx.conf

RUN sed -i "s|DatabaseHostName=.*|DatabaseHostName=tfb-database|g" /cutelyst.ini
RUN sed -i "s|DatabaseHostName=.*|DatabaseHostName=tfb-database|g" /cutelyst_socket.ini

RUN sed -i "s|SendDate=.*|SendDate=false|g" /cutelyst_socket.ini

ENV C_THREADS 1
ENV CPU_AFFINITY 1

CMD nginx -c /nginx.conf && uwsgi \
    --ini /cutelyst_socket.ini \
    --plugin /usr/lib/uwsgi/plugins/cutelyst2_plugin.so \
    --cutelyst-app ${CUTELYST_APP} \
    --processes=$(nproc) \
    --threads=${C_THREADS} \
    --cpu-affinity=${CPU_AFFINITY} \
    --reuse-port
