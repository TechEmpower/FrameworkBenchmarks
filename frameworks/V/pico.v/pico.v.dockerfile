FROM debian:sid

WORKDIR /app

RUN apt-get update -yqq && apt-get install wget unzip build-essential ca-certificates git --no-install-recommends --yes -yqq
RUN wget -q https://github.com/vlang/v/releases/download/0.1.24/v_linux.zip && unzip -q v_linux.zip -d ./v/ && rm v_linux.zip

COPY ./main.v run.sh ./

RUN git clone https://github.com/S-YOU/pico.v src && cd src && git checkout v0.0.3
RUN cd src/picoev && git clone https://github.com/S-YOU/picoev src && cd src && git checkout 311fb2c2a148baca7b7fad33afa94e4558c63478
RUN cd src/picohttpparser && git clone https://github.com/h2o/picohttpparser src && cd src && git checkout 81fe3d99fd90a55cafb993e53fd3000dbc4d564c
RUN mkdir -p /root/.vmodules/ && ln -s /app/src /root/.vmodules/syou && ./v/v -prod -cflags '-std=gnu11 -Wall -O3 -march=native -mtune=native -flto' main.v

CMD sh run.sh
