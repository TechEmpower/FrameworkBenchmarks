FROM haskell:8.10.7

# Fix Debian Buster repositories (moved to archive)
RUN sed -i 's/deb.debian.org/archive.debian.org/g' /etc/apt/sources.list && \
    sed -i 's/security.debian.org/archive.debian.org/g' /etc/apt/sources.list && \
    sed -i '/buster-updates/d' /etc/apt/sources.list

RUN apt-get update -yqq && apt-get install -yqq xz-utils make
RUN apt-get install -yqq libpq-dev

ADD ./ /spock
WORKDIR spock

RUN stack --allow-different-user build --install-ghc

EXPOSE 3000

CMD stack --allow-different-user exec spock-exe -- +RTS -A32m -N$(nproc)
