FROM ubuntu:24.04

RUN apt-get update && apt-get install -y wget xz-utils build-essential git curl && rm -rf /var/lib/apt/lists/*

# Zig 0.15.2
RUN ARCH=$(uname -m) && \
    wget -q "https://ziglang.org/download/0.15.2/zig-${ARCH}-linux-0.15.2.tar.xz" && \
    tar xf zig-*.tar.xz && mv zig-*-linux-0.15.2 /opt/zig && rm zig-*.tar.xz
ENV PATH="/opt/zig:$PATH"

# uv + Python 3.14t free-threaded
RUN curl -LsSf https://astral.sh/uv/install.sh | sh
ENV PATH="/root/.local/bin:$PATH"
RUN uv python install 3.14t && uv venv --python 3.14t /venv
ENV PATH="/venv/bin:$PATH" VIRTUAL_ENV="/venv"

WORKDIR /turboapi

RUN git clone --depth 1 --branch v1.0.17 https://github.com/justrach/turboAPI.git . && \
    git clone --depth 1 --branch v1.2.1 https://github.com/justrach/dhi.git /dhi

# Install Python deps
RUN uv pip install dhi && uv pip install -e ./python

# Build Zig backend + copy .so with correct SOABI name
RUN python zig/build_turbonet.py && \
    SOABI=$(python -c "import sysconfig; print(sysconfig.get_config_var('SOABI'))") && \
    cp zig/zig-out/lib/libturbonet.so "python/turboapi/turbonet.${SOABI}.so"

COPY app.py /turboapi/app.py

EXPOSE 8080

CMD ["/venv/bin/python", "/turboapi/app.py"]
