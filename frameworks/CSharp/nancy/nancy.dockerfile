FROM tfb/mono:latest

ADD ./ /nancy
WORKDIR /nancy

RUN xbuild src/NancyBenchmark.csproj /t:Clean
RUN xbuild src/NancyBenchmark.csproj /p:Configuration=Release

CMD bash run.sh
