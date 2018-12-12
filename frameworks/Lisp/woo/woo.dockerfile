FROM fukamachi/roswell
# FROM momonga/roswell-crossbuild:x86_64-pc-linux-gnu # don't use this piece of shit
# FROM lokedhs/sbcl-quicklisp

# Install & setup Reswell
# ENV libs 'automake libcurl4-gnutls-dev make gcc curl bzip2 locales sudo libev-dev'
RUN apt update -y
RUN apt install -y libev-dev
# RUN apt install -y ${libs}

# RUN ros install quicklisp
# RUN ls /root

# ENV rowell_archive_url 'https://github.com/roswell/roswell/archive/release.tar.gz'
# RUN echo 'install roswell' \
#   && curl -SL ${rowell_archive_url} \
#   | tar -xzC /tmp/ \
#   && cd /tmp/roswell-release \
#   && sh bootstrap \
#   && ./configure \
#   && make \
#   && make install \
#   && rm -rf /tmp/roswell-release

# RUN sudo ros setup
# ENV PATH /root/.roswell/bin:/usr/local/bin:$PATH

WORKDIR /woo
ADD  . .

RUN ["chmod", "+x", "./woo.ros"]

# RUN sbcl --noinform \
#   --noprint \
#   --non-interactive \
#   --eval "(ql:update-client)"

CMD ./woo.ros --worker $(nproc) --port 8080
