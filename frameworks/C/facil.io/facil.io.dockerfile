FROM gcc:6.4.0

COPY ./ ./

# using a common installation script will allow implementation variations (in future updates)
RUN ./setup-common.sh

# we don't need more than 32K concurrent connections
ENV CFLAGS="-DFIO_MAX_SOCK_CAPACITY=65536"

# compile test
RUN cp -f bench_app.c facil_app/src/app.c

# Build the app
RUN cd facil_app && make -j build

# Run the app
CMD ./facil_app/tmp/fioapp -p 8080 -db "tfb-database" -w -2 -t 2
