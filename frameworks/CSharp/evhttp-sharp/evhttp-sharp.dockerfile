FROM techempower/mono:0.1

ADD ./ /evhttp
WORKDIR /evhttp

RUN xbuild src/EvHttpSharpBenchmark.csproj /t:Clean
RUN xbuild src/EvHttpSharpBenchmark.csproj /p:Configuration=Release

ENV MONO_GC_PARAMS nursery-size=64m

CMD mono -O=all src/bin/Release/EvHttpSharpBenchmark.exe 127.0.0.1 8085 ${CPU_COUNT}
