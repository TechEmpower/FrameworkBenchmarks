FROM thevlang/vlang:debian-dev
RUN apt update && apt install -y libpq-dev

WORKDIR /app
COPY ./main.v run.sh fortunes.html ./
RUN v -prod -cflags '-std=gnu11 -Wall -O3 -march=native -mtune=native -flto' main.v

EXPOSE 8080
CMD sh run.sh
