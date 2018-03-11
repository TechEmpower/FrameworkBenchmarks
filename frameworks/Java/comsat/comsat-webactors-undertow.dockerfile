FROM tfb/comsat-base:latest
CMD java -Dcapsule.mode=webactors-undertow -jar build/libs/comsat-0.3-capsule.jar
