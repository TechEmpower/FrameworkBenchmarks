FROM tfb/php7:latest

ADD ./ /kumbiaphp
WORKDIR /kumbiaphp

RUN git clone -b v1.0.0-rc.2 --single-branch --depth 1 https://github.com/KumbiaPHP/KumbiaPHP.git bench/app/vendor/Kumbia
RUN git clone -b v0.4.0 --single-branch --depth 1 https://github.com/KumbiaPHP/ActiveRecord.git bench/app/vendor/Kumbia/ActiveRecord
