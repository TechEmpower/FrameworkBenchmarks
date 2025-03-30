FROM thevlang/vlang:debian-dev
RUN apt update && \
    apt install -y libpq-dev default-libmysqlclient-dev

WORKDIR /app
COPY ./veb-pg/* ./
RUN v -prod -cc clang -cflags '-std=gnu11 -Wall -O3 -march=native -mtune=native -flto' -o veb .

EXPOSE 8080
CMD ./veb
