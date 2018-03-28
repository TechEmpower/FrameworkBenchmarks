FROM techempower/yocto_http-base:0.1

WORKDIR /yocto_http

CMD api_hour -ac hello:Container
