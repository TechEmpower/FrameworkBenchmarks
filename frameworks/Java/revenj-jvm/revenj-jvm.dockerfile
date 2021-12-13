FROM maven:3.6.1-jdk-11-slim as maven
WORKDIR /revenj-jvm
COPY src src
COPY pom.xml pom.xml

# no GPG by default in the base image
RUN apt-get update -qqy && apt-get install -yqq gnupg unzip wget > /dev/null
RUN apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys 3FA7E0328081BFF6A14DA29AA6A19B38D3D831EF
RUN echo "deb http://download.mono-project.com/repo/debian wheezy main" | tee /etc/apt/sources.list.d/mono-xamarin.list
RUN apt-get update
RUN apt-get install -yqq mono-complete mono-fastcgi-server

RUN wget -q https://github.com/ngs-doo/revenj/releases/download/1.4.2/dsl-compiler.zip
RUN unzip -o dsl-compiler.zip
RUN rm dsl-compiler.zip
RUN mvn compile war:war -q

FROM openjdk:11.0.3-jdk-stretch
WORKDIR /resin
RUN curl -sL http://caucho.com/download/resin-4.0.61.tar.gz | tar xz --strip-components=1
RUN rm -rf webapps/*
COPY --from=maven /revenj-jvm/target/revenj.war webapps/ROOT.war
COPY resin.xml conf/resin.xml

EXPOSE 8080

CMD ["java", "-jar", "lib/resin.jar", "console"]
