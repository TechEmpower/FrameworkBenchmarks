FROM tfb/yocto_http-base:latest

WORKDIR /yocto_http

CMD api_hour -ac hello:Container
