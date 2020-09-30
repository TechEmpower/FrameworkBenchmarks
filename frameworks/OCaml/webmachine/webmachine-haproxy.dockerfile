FROM fedora:32

#ARG BENCHMARK_ENV=local

ENV VERSION 4.11.1
#ENV BENCHMARK_ENV $BENCHMARK_ENV

WORKDIR /webmachine

RUN dnf install --assumeyes opam diffutils postgresql-devel
RUN opam init --disable-sandboxing --auto-setup --compiler ${VERSION}
# uncomment this line to take better advantage of docker build cache when developing
#RUN opam install --yes dune webmachine caqti caqti-lwt caqti-driver-postgresql cohttp-lwt-unix ptime ezjsonm lwt_ppx

COPY ./src /webmachine
# comment the below line while developing to make better use of the docker build cache
RUN opam install --yes --deps-only ./webmachine-tfb.opam

RUN eval $(opam env) ; dune build --profile release tfb.exe

# try to keep everything above here in sync with webmachine.dockerfile for more efficent use of docker build cache
RUN dnf install --assumeyes haproxy
COPY haproxy.cfg /etc/haproxy/haproxy.cfg
COPY start-servers.sh ./start-servers.sh
RUN chmod +x /webmachine/start-servers.sh

CMD /webmachine/start-servers.sh ; /usr/sbin/haproxy -Ws -f /etc/haproxy/haproxy.cfg -p /run/haproxy.pid -q
