FROM jamesdbloom/docker-java8-maven

COPY ./ /pippo

WORKDIR /pippo

CMD ["mvn","compile","exec:java"]
# CMD mvn compile exec:java