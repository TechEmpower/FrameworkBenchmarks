FROM tfb/mono:latest

ADD ./ /servicestack
WORKDIR /servicestack

RUN mkdir lib
RUN curl -sL -O https://dist.nuget.org/win-x86-commandline/latest/nuget.exe
RUN mono nuget.exe install src/packages.config -OutputDirectory lib/
RUN xbuild src/ServiceStackBenchmark.csproj /t:Clean
RUN xbuild src/ServiceStackBenchmark.csproj /t:Build

CMD bash run.sh
