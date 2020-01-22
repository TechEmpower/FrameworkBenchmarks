# ================================
# Build image
# ================================
FROM vapor/swift:5.1 as build
WORKDIR /build

# Copy entire repo into container
COPY ./app .

# Compile with optimizations
RUN swift build \
	--enable-test-discovery \
	-c release

# ================================
# Run image
# ================================
FROM vapor/ubuntu:18.04
WORKDIR /run

# Copy build artifacts
COPY --from=build /build/.build/release /run

# Copy Swift runtime libraries
COPY --from=build /usr/lib/swift/ /usr/lib/swift/

ENTRYPOINT ["./app", "serve", "--env", "production", "--hostname", "0.0.0.0", "--port", "8080"]
