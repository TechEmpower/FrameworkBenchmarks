FROM errantmind/debian-faf:v2

COPY ./Cargo.toml ./Cargo.toml
COPY ./src ./src
COPY ./merged.profdata ./merged.profdata
ENV CC=/usr/bin/clang-12
ENV CXX=/usr/bin/clang++-12
RUN /root/.cargo/bin/cargo update
RUN RUSTFLAGS="-Ctarget-cpu=native -Clinker=/usr/bin/clang-12 -Clink-arg=-fuse-ld=lld-12 -Clink-arg=-flto=thin \
   -Clto=thin -Cembed-bitcode=yes -Copt-level=3 -Ccodegen-units=1 -Cforce-frame-pointers=n -Cprofile-use=/faf/merged.profdata" \
   /root/.cargo/bin/cargo build --verbose --release && strip --strip-all target/release/faf-ex

EXPOSE 8089
CMD ./target/release/faf-ex
