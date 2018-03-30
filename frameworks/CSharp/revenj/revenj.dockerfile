FROM tfb/java:latest as java
FROM tfb/mono:latest
COPY --from=java /java /java
ENV JAVA_HOME=/java/jdk-10
ENV PATH="${JAVA_HOME}/bin:${PATH}"

ADD ./ /revenj
WORKDIR /revenj

RUN curl -sL -O https://github.com/ngs-doo/revenj/releases/download/1.4.2/dsl-compiler.zip
RUN unzip dsl-compiler.zip
RUN rm dsl-compiler.zip

RUN curl -sL -O https://github.com/ngs-doo/dsl-compiler-client/releases/download/1.8.2/dsl-clc.jar
RUN curl -sL -O https://github.com/ngs-doo/revenj/releases/download/1.4.2/http-server.zip
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

CMD mono /revenj/exe/Revenj.Http.exe
