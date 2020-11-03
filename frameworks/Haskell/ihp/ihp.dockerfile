FROM nixos/nix

COPY ./src /ihp
WORKDIR /ihp

# Add build dependencies
RUN nix-env -i git cachix
RUN cachix use digitallyinduced

# Build 
RUN nix-shell -j auto --cores 0 --command "make build/bin/RunOptimizedProdServer"

# Setup
ENV DATABASE_URL=postgres://benchmarkdbuser:benchmarkdbpass@tfb-database:5432/hello_world
ENV PORT=8080
EXPOSE 8080

# Run
CMD nix-shell -j auto --cores 0 --command "./build/bin/RunOptimizedProdServer"
