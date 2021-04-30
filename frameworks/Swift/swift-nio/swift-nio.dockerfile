# ================================
# Build image
# ================================
FROM swift:5.3 as build
WORKDIR /build

# Copy entire repo into container
COPY ./app .

# Compile with optimizations
RUN swift build \
	--enable-test-discovery \
	-c release \
	-Xswiftc -enforce-exclusivity=unchecked

# ================================
# Run image
# ================================
FROM swift:5.3-slim
WORKDIR /run

# Install Swift dependencies
RUN apt-get -qq update && DEBIAN_FRONTEND=noninteractive apt-get install -y \
  libatomic1 \
  && rm -r /var/lib/apt/lists/*

# Copy build artifacts
COPY --from=build /build/.build/release /run

# Copy Swift runtime libraries
COPY --from=build /usr/lib/swift/ /usr/lib/swift/

EXPOSE 8080

ENTRYPOINT ["./app"]
