FROM debian:bookworm-slim

ARG LDC_VERSION=1.41.0
ARG JUPTUNE_REF=52294fa45912dacac24efc80b4a2fad8db958841
ARG TFB_TEST_NAME

ENV TEST_NAME=${TFB_TEST_NAME}

# Install system deps & LDC
RUN apt update \
    && apt install -y curl xz-utils gnupg libsodium-dev meson unzip pkg-config \
    && curl -fsS https://dlang.org/install.sh | bash -s ldc-${LDC_VERSION}

# Install Juptune
WORKDIR /juptune
RUN curl -fsSL https://github.com/Juptune/juptune/archive/${JUPTUNE_REF}.zip -o code.zip \
    && unzip code.zip \
    && cd juptune* \
    && . ~/dlang/ldc-${LDC_VERSION}/activate \
    && meson setup build --buildtype debugoptimized -Ddefault_library=static \
    && meson install -C build

# Compile everything
WORKDIR /app
COPY ./src/ .
RUN . ~/dlang/ldc-${LDC_VERSION}/activate \
    && meson setup build --buildtype debugoptimized -Ddefault_library=static \
    && meson compile -C build

ENTRYPOINT [ "/app/build/juptune-tfb" ]