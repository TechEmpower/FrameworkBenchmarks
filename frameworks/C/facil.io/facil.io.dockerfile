FROM tfb/base:latest

COPY ./ ./

# using a common installation script will allow implementation variations (in future updates)
RUN ./setup-common.sh

# set compiler to gcc-6 (avoid gcc-4.8.4)... do we need this?
# ENV CC="gcc-6"

# we don't need more than 32K concurrent connections
ENV CFLAGS="-DLIB_SOCK_MAX_CAPACITY=32768"

# compile test
RUN cp -f bench_app.c facil_app/src/app.c

# Build the app
RUN cd facil_app && make -j build

# Set Worker (process) count
ENV WORKERS=$((CPU_COUNT / 8))

# Run the app
CMD ["./facil_app/tmp/demo", "-p", "8080", "-db \"TFB-database\"", "-w", ${WORKERS:--1}, ${WORKERS:+-t=8}]
