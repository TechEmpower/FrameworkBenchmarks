FROM tfb/comsat-base:latest
CMD java -Dcapsule.mode=webactors-netty -jar build/libs/comsat-0.3-capsule.jar
