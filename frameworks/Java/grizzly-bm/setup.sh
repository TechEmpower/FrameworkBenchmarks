#!/bin/bash

fw_depends java7 maven

mvn clean compile assembly:single

cd target
java -Dorg.glassfish.grizzly.nio.transport.TCPNIOTransport.max-receive-buffer-size=16384 -Dorg.glassfish.grizzly.http.io.OutputBuffer.default-buffer-size=1024 -Dorg.glassfish.grizzly.memory.BuffersBuffer.bb-cache-size=32 -jar grizzly-bm-0.1-jar-with-dependencies.jar &
