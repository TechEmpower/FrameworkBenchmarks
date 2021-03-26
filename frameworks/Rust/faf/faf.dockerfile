FROM errantmind/debian-faf

COPY ./Cargo.toml ./Cargo.toml
COPY ./src ./src
COPY ./merged.profdata ./merged.profdata
# RUN export PATH="/root/.cargo/bin:$PATH"
#RUN export CC=/usr/bin/clang-12 && alias clang="/usr/bin/clang-12" && alias clang-format="clang-format-12"
RUN /root/.cargo/bin/cargo update
RUN RUSTFLAGS="-Ctarget-cpu=native -Clinker=/usr/bin/clang-12 -Clink-arg=-fuse-ld=lld-12 -Clink-arg=-flto=thin \
   -Clto=thin -Cembed-bitcode=yes -Copt-level=3 -Ccodegen-units=1 -Cforce-frame-pointers=n -Cprofile-use=/faf/merged.profdata" \
   /root/.cargo/bin/cargo build --verbose --release -Z build-std --target=x86_64-unknown-linux-gnu \
   && strip --strip-all target/x86_64-unknown-linux-gnu/release/faf

EXPOSE 8089
CMD ./target/x86_64-unknown-linux-gnu/release/faf
