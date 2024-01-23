FROM nixos/nix

COPY ./src /ihp
WORKDIR /ihp

# Add build dependencies
RUN nix-env -i cachix
RUN cachix use digitallyinduced

# Build 
RUN nix-build -j auto --cores 0

# Setup
ENV DATABASE_URL=postgres://benchmarkdbuser:benchmarkdbpass@tfb-database:5432/hello_world
ENV PORT=8080
EXPOSE 8080

# Run
CMD ./result/bin/RunProdServer +RTS -A32m -N$(nproc) -qn2 -M2G -RTS
