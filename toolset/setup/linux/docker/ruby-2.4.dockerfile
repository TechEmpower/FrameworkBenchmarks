FROM rvm:latest

RUN apt install -yqq llvm-dev libjemalloc-dev

ENV MRI_VERSION 2.4.2

RUN rvm install $MRI_VERSION -C --with-jemalloc
RUN rvm $MRI_VERSION do gem install bundler -v 1.15.4
