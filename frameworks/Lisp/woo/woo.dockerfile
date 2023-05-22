FROM debian:bullseye-slim AS debian

ENV DEBIAN_FRONTEND noninteractive
ENV TERM linux
ENV ROS_VERSION 22.12.14.113
ENV LISP sbcl-bin/2.3.4
ENV ASDF_VERSION 3.3.6
ENV QUICKLISP_VERSION 2023-02-15

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
    && curl -L -O https://github.com/roswell/roswell/releases/download/v${ROS_VERSION}/roswell_${ROS_VERSION}-1_amd64.deb \
    && dpkg -i roswell_${ROS_VERSION}-1_amd64.deb \
    && ros quicklisp.dist=http://beta.quicklisp.org/dist/quicklisp/${QUICKLISP_VERSION}/distinfo.txt setup \
    && ros install ${LISP} \ 
    && ros use ${LISP} \ 
    && ros install asdf/${ASDF_VERSION} \
    && ros config \
    && rm roswell_${ROS_VERSION}-1_amd64.deb

RUN echo 'export PATH=$HOME/.roswell/bin:$PATH' >> ~/.bashrc


FROM roswell AS builder

RUN apt-get update -q \
    && apt-get install --no-install-recommends -q -y \
         git \
         build-essential \
         libev-dev \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /woo
ADD  . .

RUN mkdir -p /libs && \
    git clone https://github.com/svetlyak40wt/fast-http /libs/fast-http && \
    cd /libs/fast-http && \
    git checkout http-pipelining && \
    git clone https://github.com/svetlyak40wt/woo /libs/woo && \
    cd /libs/woo && \
    git checkout fix-error-codes

RUN CL_SOURCE_REGISTRY=/libs// ros build woo.ros


FROM debian

RUN apt-get update -q \
    && apt-get install --no-install-recommends -q -y \
         libev4 \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /woo
COPY --from=builder /woo/woo .

RUN ["chmod", "+x", "./woo"]

EXPOSE 8080

CMD ./woo --worker $(nproc) --address 0.0.0.0 --port 8080
