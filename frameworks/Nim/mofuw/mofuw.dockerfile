FROM tfb/nimble:latest

RUN git clone https://github.com/2vg/mofuw.git && \
    cd mofuw && \
    echo 'y' | nimble install

COPY ./ ./

RUN nim c -d:release --threads:on techempower.nim

CMD ./techempower