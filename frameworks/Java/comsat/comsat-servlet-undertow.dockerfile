FROM tfb/comsat-base:latest
CMD java -Dcapsule.mode=servlet-undertow -jar build/libs/comsat-0.3-capsule.jar
