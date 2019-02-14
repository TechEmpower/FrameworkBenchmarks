FROM jamesdbloom/docker-java8-maven

COPY ./ /pippo

WORKDIR /pippo

EXPOSE 8000

CMD ["mvn","compile","exec:java"]
# CMD mvn compile exec:java