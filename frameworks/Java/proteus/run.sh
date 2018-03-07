#!/bin/bash
java \
  -Dlogback.configurationFile="conf/logback.xml" \
  -Dconfig.file="conf/application.conf" \
  -Xms2g \
  -Xmx2g \
  -XX:+AggressiveOpts \
  -server \
  -XX:-UseBiasedLocking \
  -XX:+UseStringDeduplication \
  -Djava.net.preferIPv4Stack=true \
  -classpath "/proteus/target/proteus-techempower-1.0.0.jar:/proteus/target/lib/*" \
  io.sinistral.ExampleApplication
