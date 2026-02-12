FROM debian:bookworm

ARG LDC_VERSION=1.41.0
ARG JUPTUNE_REF=1b012d22d4e5e29e88fdf760be4cfa899ce3dfb2

# Install system deps & LDC
RUN apt update \
    && apt install -y curl xz-utils gnupg libsodium-dev meson unzip pkg-config clang cmake libssl-dev \
    && curl -fsS https://dlang.org/install.sh | bash -s ldc-${LDC_VERSION}

# Install Juptune (Bookworm's meson is kind of old so we have to rename meson.options, 
#                  and some of the dependencies are older than what Juptune targets, but downgrading is fine for the benchmark's code.)
#
# I'm also trying to dummy out some executables from being built so the entire thing builds faster... need to add an option upstream to make that better.
WORKDIR /juptune
RUN curl -fsSL https://github.com/Juptune/juptune/archive/${JUPTUNE_REF}.zip -o code.zip \
    && unzip code.zip \
    && cd juptune* \
    && . ~/dlang/ldc-${LDC_VERSION}/activate \
    && mv meson.options meson_options.txt \
    && sed -i 's/1.0.20/1.0.18/' meson.build \
    && sed -i 's/3.5.0/1.0.18/' meson.build \
    && sed -iE "s|.+./examples|#|" meson.build \
    && sed -iE "s|^subdir|#|" meson.build \
    && sed -iEz 's|juptune_all_unittest_exe[^)]+)|#|' meson.build \
    && meson setup build --buildtype release -Dlightweight-results=true -Ddefault_library=static \
    && meson install -C build

# Compile everything
WORKDIR /app
COPY ./src/ .
RUN . ~/dlang/ldc-${LDC_VERSION}/activate \
    && meson setup build --buildtype release -Ddefault_library=static \
    && meson compile -C build

ENTRYPOINT [ "/app/build/juptune-tfb" ]