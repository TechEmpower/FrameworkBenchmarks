FROM tfb/compojure-base:latest
CMD java -jar ${RESIN_HOME}/lib/resin.jar console
