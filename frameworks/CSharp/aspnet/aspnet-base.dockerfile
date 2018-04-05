FROM techempower/mono:0.1

ADD ./ /aspnet
WORKDIR /aspnet

RUN xbuild src/Benchmarks.build.proj /t:Clean
RUN xbuild src/Benchmarks.build.proj /t:Build
