FROM mono:6.12.0.122
RUN apt-get update -yqq && apt-get install -yqq unzip

RUN mkdir /java
WORKDIR /java
RUN curl -sL -O https://download.java.net/java/GA/jdk10/10/binaries/openjdk-10_linux-x64_bin.tar.gz
RUN tar xf openjdk-10_linux-x64_bin.tar.gz
ENV JAVA_HOME=/java/jdk-10
ENV PATH ${JAVA_HOME}/bin:${PATH}

WORKDIR /revenj
COPY Revenj.Bench Revenj.Bench
COPY Revenj.Bench.sln Reveng.Bench.sln
COPY Revenj.Http.exe.config Revenj.Http.exe.config

RUN curl -sL -O https://github.com/ngs-doo/revenj/releases/download/v1.5.0/dsl-compiler.zip
RUN unzip dsl-compiler.zip
RUN rm dsl-compiler.zip

RUN curl -sL -O https://github.com/ngs-doo/dsl-compiler-client/releases/download/clc-v1.9.6/dsl-clc.jar
RUN curl -sL -O https://github.com/ngs-doo/revenj/releases/download/v1.5.0/http-server.zip
RUN unzip http-server.zip -d /revenj/exe

RUN java -jar dsl-clc.jar \
	temp=/revenj/tmp/ \
	force \
	dsl=/revenj/Revenj.Bench \
	manual-json \
	compiler=/revenj/dsl-compiler.exe \
	revenj.net=/revenj/exe/ServerModel.dll \
	no-prompt \
	dependencies:revenj.net=/revenj/exe

RUN xbuild /revenj/Revenj.Bench/Revenj.Bench.csproj /t:Rebuild /p:Configuration=Release

RUN mv /revenj/Revenj.Http.exe.config /revenj/exe/Revenj.Http.exe.config

EXPOSE 8080

CMD ["mono", "/revenj/exe/Revenj.Http.exe"]
