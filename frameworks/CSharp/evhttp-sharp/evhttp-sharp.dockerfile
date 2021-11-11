FROM mono:5.10.0.160
RUN apt-get update -yqq && apt-get install -yqq libevent-dev

WORKDIR /evhttp
COPY src src

RUN xbuild src/EvHttpSharpBenchmark.csproj /t:Clean
RUN xbuild src/EvHttpSharpBenchmark.csproj /p:Configuration=Release

ENV MONO_GC_PARAMS nursery-size=64m

EXPOSE 8085

CMD mono -O=all src/bin/Release/EvHttpSharpBenchmark.exe 127.0.0.1 8085 $(nproc)
