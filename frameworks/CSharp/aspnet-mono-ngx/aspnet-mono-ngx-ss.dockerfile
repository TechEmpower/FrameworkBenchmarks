FROM mono:5.10.0.160
RUN apt update -yqq && apt install -yqq nginx wget mono-fastcgi-server

WORKDIR /aspnet
COPY src src
COPY nginx.conf nginx.conf
COPY run.sh run.sh

RUN xbuild src/Benchmarks.build.proj /t:Clean
RUN xbuild src/Benchmarks.build.proj /t:Build

CMD bash run.sh
