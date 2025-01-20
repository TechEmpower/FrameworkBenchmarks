FROM ubuntu:24.04
RUN apt-get update -yqq && apt-get install -yqq apt-utils software-properties-common wget unzip cmake git
RUN apt-get install -yqq gcc g++ openssl libssl-dev zlib1g-dev build-essential locales

RUN apt-get -y install brotli libbrotli-dev 
RUN apt-get -y install libreadline-dev 
RUN apt-get -y install mysql-client
RUN apt-get -y install libmysqlclient-dev

RUN locale-gen en_US.UTF-8
ENV LANG en_US.UTF-8
ENV LANGUAGE en_US:en
ENV LC_ALL en_US.UTF-8

COPY ./ ./
WORKDIR /

# RUN wget https://github.com/hggq/paozhu/releases/download/v1.5.8/benchmark.zip
RUN git clone https://github.com/hggq/paozhu
# RUN unzip benchmark.zip
# RUN rm -Rf ./paozhu/controller
# RUN rm -Rf ./paozhu/libs
# RUN rm -Rf ./paozhu/view
# RUN rm -Rf ./paozhu/viewsrc
# RUN rm -Rf ./paozhu/orm
# RUN rm -Rf ./paozhu/models
# RUN rm -Rf ./paozhu/common



# COPY ./paozhu_benchmark/controller ./paozhu/
# COPY ./paozhu_benchmark/libs ./paozhu/
# COPY ./paozhu_benchmark/view ./paozhu/
# COPY ./paozhu_benchmark/viewsrc ./paozhu/

# COPY ./paozhu_benchmark/orm ./paozhu/
# COPY ./paozhu_benchmark/models ./paozhu/
# COPY ./paozhu_benchmark/common ./paozhu/

# RUN ls -l ./paozhu
# RUN pwd
# RUN mkdir ./paozhu/common
# RUN mkdir ./paozhu/libs
# RUN mkdir ./paozhu/libs/types
# COPY ./paozhu_benchmark/libs/types/techempower_json.h ./paozhu/libs/types/
# COPY ./paozhu_benchmark/libs/types/techempower_json_jsonreflect.cpp ./paozhu/libs/types/

# RUN mkdir ./paozhu/controller
# RUN mkdir ./paozhu/controller/include
# RUN mkdir ./paozhu/controller/src

# COPY ./paozhu_benchmark/controller/include/techempower.h ./paozhu/controller/include/
# COPY ./paozhu_benchmark/controller/src/techempower.cpp ./paozhu/controller/src/


# COPY ./paozhu_benchmark/common/autocontrolmethod.hpp ./paozhu/common/
# COPY ./paozhu_benchmark/common/reghttpmethod_pre.hpp ./paozhu/common/
# COPY ./paozhu_benchmark/common/reghttpmethod.hpp ./paozhu/common/
# COPY ./paozhu_benchmark/common/json_reflect_headers.h ./paozhu/common/

# COPY ./paozhu_benchmark/common/cost_define.h ./paozhu/common/
# COPY ./paozhu_benchmark/common/autorestfulpaths.hpp ./paozhu/common/
# COPY ./paozhu_benchmark/common/websockets_method_reg.hpp ./paozhu/common/
# COPY ./paozhu_benchmark/common/httphook.cpp ./paozhu/common/

COPY ./paozhu_benchmark/conf/server.conf ./paozhu/conf/server.conf
COPY ./paozhu_benchmark/conf/orm.conf ./paozhu/conf/orm.conf

# must use testbenchmark.cpp to test benchmark
COPY ./paozhu_benchmark/CMakeLists.txt ./paozhu/CMakeLists.txt

# RUN mkdir ./paozhu/view
# RUN mkdir ./paozhu/view/techempower

# COPY ./paozhu_benchmark/view/techempower/fortunes.html ./paozhu/view/techempower/

# RUN mkdir ./paozhu/viewsrc
# RUN mkdir ./paozhu/viewsrc/include
# RUN mkdir ./paozhu/viewsrc/view
# RUN mkdir ./paozhu/viewsrc/view/techempower

# COPY ./paozhu_benchmark/viewsrc/view/techempower/fortunes.cpp ./paozhu/viewsrc/view/techempower/
# COPY ./paozhu_benchmark/viewsrc/include/viewsrc.h ./paozhu/viewsrc/include/
# COPY ./paozhu_benchmark/viewsrc/include/regviewmethod.hpp ./paozhu/viewsrc/include/



# RUN mkdir ./paozhu/orm
# RUN mkdir ./paozhu/orm/include


# COPY ./paozhu_benchmark/orm/orm.h ./paozhu/orm/
# COPY ./paozhu_benchmark/orm/include/fortunebase.h ./paozhu/orm/include/
# COPY ./paozhu_benchmark/orm/include/worldbase.h ./paozhu/orm/include/

# RUN mkdir ./paozhu/models
# RUN mkdir ./paozhu/models/include

# COPY ./paozhu_benchmark/models/include/Fortune.h ./paozhu/models/include/
# COPY ./paozhu_benchmark/models/include/World.h ./paozhu/models/include/
# COPY ./paozhu_benchmark/models/World.cpp ./paozhu/models/
# COPY ./paozhu_benchmark/models/Fortune.cpp ./paozhu/models/

WORKDIR /paozhu
RUN unzip asio.zip

RUN cmake . -B build -DCMAKE_BUILD_TYPE=Release 
RUN cmake --build build

EXPOSE 8888

CMD ./bin/paozhu