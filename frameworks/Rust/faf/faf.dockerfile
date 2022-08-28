FROM errantmind/debian-faf:v3

COPY ./Cargo.toml ./Cargo.toml
COPY ./src ./src
ENV CC=/usr/bin/clang-13
ENV CXX=/usr/bin/clang++-13
RUN /root/.cargo/bin/cargo update
RUN RUSTFLAGS="-Ctarget-cpu=native -Ztune-cpu=native -Zmutable-noalias=yes -Clink-arg=-fexperimental-new-pass-manager \
   -Clinker=/usr/bin/clang-13 -Clink-arg=-fuse-ld=/usr/bin/ld.lld-13 -Clink-arg=-flto=thin -Clto=thin -Copt-level=3 \
   -Ccodegen-units=1 -Cpanic=abort -Cembed-bitcode=yes -Cforce-frame-pointers=n -Cdebug-assertions=no -Coverflow-checks=no \
   -Ccontrol-flow-guard=no -Clink-dead-code=no -Zno-parallel-llvm" \
   /root/.cargo/bin/cargo build --release --target x86_64-unknown-linux-gnu -Zbuild-std=panic_abort,core,std,alloc,proc_macro,compiler_builtins \
   && strip ./target/x86_64-unknown-linux-gnu/release/faf-ex

EXPOSE 8080
CMD ./target/x86_64-unknown-linux-gnu/release/faf-ex
