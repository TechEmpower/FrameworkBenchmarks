FROM tfb/base:latest

ENV RUST_VERSION="1.22.1"

RUN wget https://static.rust-lang.org/dist/rust-${RUST_VERSION}-x86_64-unknown-linux-gnu.tar.gz
RUN tar xvf rust-${RUST_VERSION}-x86_64-unknown-linux-gnu.tar.gz

RUN cd rust-${RUST_VERSION}-x86_64-unknown-linux-gnu && \
	./install.sh --prefix=/rust

ENV LD_LIBRARY_PATH=/rust/lib:${LD_LIBRARY_PATH}
ENV PATH=/rust/bin:${PATH}
