FROM tfb/mono:latest

ADD ./ /aspnet
WORKDIR /aspnet

RUN xbuild src/Benchmarks.build.proj /t:Clean
RUN xbuild src/Benchmarks.build.proj /t:Build
