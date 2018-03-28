FROM tfb/maven-java8:latest as maven

RUN apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys 3FA7E0328081BFF6A14DA29AA6A19B38D3D831EF
RUN echo "deb http://download.mono-project.com/repo/debian wheezy main" | tee /etc/apt/sources.list.d/mono-xamarin.list
RUN apt-get update
RUN apt-get install -y mono-complete mono-fastcgi-server

ADD ./ /revenj-jvm
WORKDIR /revenj-jvm
RUN wget -q https://github.com/ngs-doo/revenj/releases/download/1.4.2/dsl-compiler.zip
RUN unzip -o dsl-compiler.zip
RUN rm dsl-compiler.zip
RUN mvn clean compile war:war

FROM tfb/resin-java8:latest
COPY --from=maven /revenj-jvm/target/revenj.war ${RESIN_HOME}/webapps/ROOT.war
CMD java -jar ${RESIN_HOME}/lib/resin.jar console
