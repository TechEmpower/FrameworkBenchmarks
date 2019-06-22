FROM debian:stretch-slim AS debian

ARG DEBIAN_FRONTEND=noninteractive
ARG TERM=linux

RUN echo 'APT::Get::Install-Recommends "false";' > /etc/apt/apt.conf.d/00-general \
    && echo 'APT::Get::Install-Suggests "false";' >> /etc/apt/apt.conf.d/00-general \
    && echo 'APT::Get::Assume-Yes "true";' >> /etc/apt/apt.conf.d/00-general \
    && echo 'APT::Get::force-yes "true";' >> /etc/apt/apt.conf.d/00-general


FROM debian AS roswell

RUN apt-get update -q \
    && apt-get install --no-install-recommends -q -y \
         bzip2 \
         ca-certificates curl libcurl3-gnutls \
         make \
    && rm -rf /var/lib/apt/lists/* \
    && curl -L -O https://github.com/roswell/roswell/releases/download/v19.06.10.100/roswell_19.06.10.100-1_amd64.deb \
    && dpkg -i roswell_19.06.10.100-1_amd64.deb \
    && ros setup \
    && rm roswell_19.06.10.100-1_amd64.deb

RUN echo 'export PATH=$HOME/.roswell/bin:$PATH' >> ~/.bashrc


FROM roswell AS builder

RUN apt-get update -q \
    && apt-get install --no-install-recommends -q -y \
         build-essential \
         libev-dev \
         git-core \
    && rm -rf /var/lib/apt/lists/*

RUN mkdir -p ${HOME}/common-lisp/ \
    && cd ${HOME}/common-lisp/ \
    && git clone https://github.com/defunkydrummer/ninglex.git

WORKDIR /ninglex
ADD  . .

RUN ros build ninglex.ros


FROM debian

RUN apt-get update -q \
    && apt-get install --no-install-recommends -q -y \
         libev4 \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /ninglex
COPY --from=builder /ninglex/ninglex .

RUN ["chmod", "+x", "./ninglex"]

EXPOSE 8080

CMD ./ninglex --worker $(nproc) --address 0.0.0.0 --port 8080
