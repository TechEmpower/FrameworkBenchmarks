FROM syou/v-dev:0.1

WORKDIR /app

COPY ./main.v run.sh ./

RUN git clone https://github.com/S-YOU/pico.v src && cd src && git checkout v0.0.4 \
    && cd /app/src/picoev && git clone https://github.com/S-YOU/picoev src && cd src && git checkout v0.0.1 \
    && cd /app/src/picohttpparser && git clone https://github.com/S-YOU/picohttpparser src && cd src && git checkout v0.0.1 \
    && ln -s /app/src /root/.vmodules/syou \
    && cd /app && v -prod -cflags '-std=gnu11 -Wall -O3 -march=native -mtune=native -flto' main.v

EXPOSE 8088

CMD sh run.sh
