FROM techempower/maven:0.1

ADD ./ /rapidoid
WORKDIR /rapidoid
RUN mvn clean compile assembly:single
