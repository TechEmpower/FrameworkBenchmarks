FROM tfb/base:latest

COPY ./ ./

RUN mkdir facil_app && \
    cd facil_app && \
    curl -s -o facil.io.tar.gz -LJO https://api.github.com/repos/boazsegev/facil.io/tarball/0.6.0.beta.6 && \
    tar --strip-components=1 -xzf facil.io.tar.gz

# compile test
RUN mkdir facil_app/src && cp bench_app.c facil_app/src

# we don't need more than 32K concurrent connections
ENV CFLAGS="-DLIB_SOCK_MAX_CAPACITY=32768"

# Build the app
RUN cd facil_app && make -j build

# Run the app
CMD ["./facil_app/tmp/demo", "-p", "8080", "-db \"TFB-database\"", "-w", "-1", "-t", "1"]
