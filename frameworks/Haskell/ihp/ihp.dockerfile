FROM nixos/nix

# Add build dependencies
RUN nix-env -i cachix
RUN cachix use digitallyinduced

COPY ./src /ihp
WORKDIR /ihp

# Build 
RUN nix --extra-experimental-features "nix-command flakes" build -j auto --cores 0 .#optimized-prod-server

# Setup
ENV DATABASE_URL=postgres://benchmarkdbuser:benchmarkdbpass@tfb-database:5432/hello_world
ENV PORT=8080
EXPOSE 8080

# Run
CMD ./result/bin/RunProdServer +RTS -A32m -N$(nproc) -qn2 -M2G -RTS
