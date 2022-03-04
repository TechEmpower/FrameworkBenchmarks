FROM ghcr.io/graalvm/graalvm-ce:20.3.1.2
# Set working dir
RUN mkdir /app
WORKDIR /app

COPY ./package.json /app
# Get dependencies
RUN npm --unsafe-perm install
# Copy the app
COPY ./ /app
# Compile the template
RUN npm run template

EXPOSE 8080

# Run the code
CMD java \
    -server                                             \
    -XX:+UseNUMA                                        \
    -XX:+UseParallelGC                                  \
    -XX:+AggressiveOpts                                 \
    -Dvertx.disableMetrics=true                         \
    -Dvertx.disableH2c=true                             \
    -Dvertx.disableWebsockets=true                      \
    -Dvertx.flashPolicyHandler=false                    \
    -Dvertx.threadChecks=false                          \
    -Dvertx.disableContextTimings=true                  \
    -Dvertx.disableTCCL=true                            \
    -Dvertx.disableHttpHeadersValidation=true           \
    -jar node_modules/.bin/es4x-launcher.jar            \
    --instances `grep --count ^processor /proc/cpuinfo` \
    --options vertx.json
