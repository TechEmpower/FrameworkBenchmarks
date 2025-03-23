FROM thevlang/vlang:debian-dev
RUN apt update && apt install -y libpq-dev

# Compile veb
WORKDIR /app
COPY ./main.v run.sh ./
RUN v -prod -cflags '-std=gnu11 -Wall -O3 -march=native -mtune=native -flto' main.v

# Run veb
EXPOSE 8080
CMD sh run.sh
#CMD ./main