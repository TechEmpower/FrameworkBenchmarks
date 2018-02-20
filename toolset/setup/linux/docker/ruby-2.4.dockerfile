FROM rvm:latest

RUN apt install -yqq llvm-dev libjemalloc-dev

ENV MRI_VERSION 2.4.2

RUN /bin/bash -l -c ". /etc/profile.d/rvm.sh && \
      rvm install 2.4.2 -C --with-jemalloc && \
      rvm 2.4.2 do gem install bundler -v 1.15.4"
