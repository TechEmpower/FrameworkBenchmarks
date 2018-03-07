FROM tfb/comsat-base:latest
CMD java -Dcapsule.mode=servlet-jetty -jar build/libs/comsat-0.3-capsule.jar
