FROM techempower/comsat-base:0.1

CMD java -Dcapsule.mode=webactors-undertow -jar build/libs/comsat-0.3-capsule.jar
