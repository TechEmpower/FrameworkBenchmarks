FROM techempower/gradle-java8:0.1

ADD ./ /wizzardo-http
WORKDIR /wizzardo-http
RUN gradle --refresh-dependencies clean fatJar