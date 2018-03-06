FROM tfb/maven:latest
ADD ./ /grizzly
WORKDIR /grizzly
RUN mvn clean compile assembly:single
CMD java \
    -Dorg.glassfish.grizzly.nio.transport.TCPNIOTransport.max-receive-buffer-size=16384 \
    -Dorg.glassfish.grizzly.http.io.OutputBuffer.default-buffer-size=1024 \
    -Dorg.glassfish.grizzly.memory.BuffersBuffer.bb-cache-size=32 \
    -jar target/grizzly-bm-0.1-jar-with-dependencies.jar
