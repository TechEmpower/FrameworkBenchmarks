FROM rust:1.57.0

# Disable simd at jsonescape
# ENV CARGO_CFG_JSONESCAPE_DISABLE_AUTO_SIMD=

RUN apt-get update -yqq && apt-get install -yqq cmake g++

ADD ./ /ntex
WORKDIR /ntex
ENV CC=/usr/bin/clang-13
ENV CXX=/usr/bin/clang++-13
RUN cargo update
RUN cargo clean
RUN RUSTFLAGS="-Ctarget-cpu=native -Ztune-cpu=native -Zmutable-noalias=yes -Clink-arg=-fexperimental-new-pass-manager \
   -Clinker=/usr/bin/clang-13 -Clink-arg=-fuse-ld=/usr/bin/ld.lld-13 -Clink-arg=-flto=thin -Clto=thin -Copt-level=3 \
   -Ccodegen-units=1 -Cpanic=abort -Cembed-bitcode=yes -Cforce-frame-pointers=n -Cdebug-assertions=no -Coverflow-checks=no \
   -Ccontrol-flow-guard=no -Clink-dead-code=no -Zno-parallel-llvm" \
   cargo build --release --target x86_64-unknown-linux-gnu -Zbuild-std=panic_abort,core,std,alloc,proc_macro,compiler_builtins

EXPOSE 8080

CMD ./target/release/ntex-db
