FROM mono:5.8.0.127
RUN apt-get update -yqq && apt-get install -yqq nginx wget mono-fastcgi-server

WORKDIR /servicestack
COPY src src
COPY nginx.conf nginx.conf
COPY run.sh run.sh

RUN mkdir lib
RUN curl -sL -O https://dist.nuget.org/win-x86-commandline/latest/nuget.exe
RUN mono nuget.exe install src/packages.config -OutputDirectory lib/
RUN xbuild src/ServiceStackBenchmark.csproj /t:Clean
RUN xbuild src/ServiceStackBenchmark.csproj /t:Build

EXPOSE 8080

CMD bash run.sh
