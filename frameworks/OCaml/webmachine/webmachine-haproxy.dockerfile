FROM ocaml/opam:debian-10-ocaml-4.12

ENV DIR project
# https://blog.packagecloud.io/eng/2017/02/21/set-environment-variable-save-thousands-of-system-calls/
ENV TZ  :/etc/localtime

# https://caml.inria.fr/pub/docs/manual-ocaml/libref/Gc.html
# https://linux.die.net/man/1/ocamlrun
# https://blog.janestreet.com/memory-allocator-showdown/
ENV OCAMLRUNPARAM a=2,o=240

ENV PKGS="\
atdgen>=2.2.1 \
atdgen-runtime>=2.2.1 \
caqti>=1.6.0 \
caqti-driver-postgresql>=1.6.0 \
caqti-lwt>=1.6.0 \
cohttp-lwt-unix>=4.0.0 \
conf-libev>=4-12 \
dune>=2.8.5 \
httpaf>=0.7.1 \
httpaf-lwt-unix>=0.7.1 \
lwt>=5.4.1 \
lwt_ppx>=2.0.2 \
opium>=0.20.0 \
ppx_deriving_yojson>=3.6.1 \
ppx_rapper>=3.0.0 \
tiny_httpd>=0.8 \
tyxml>=4.5.0 \
webmachine>=0.7.0 \
yojson>=1.7.0 \
"

RUN \
  opam update && \
  opam depext $PKGS && \
  opam install $PKGS

WORKDIR /${DIR}

COPY ./src /${DIR}

RUN sudo chown -R opam: . && make build

EXPOSE 8080

# try to keep everything above here in sync with other dockerfiles in project for more efficent use of docker build cache

# This makes the program only spawn one child process to serve requests
ENV APP_INSTANCES 1

USER root

RUN apt-get update && apt-get install -y haproxy
COPY haproxy.cfg /etc/haproxy/haproxy.cfg
COPY start-servers.sh ./start-servers.sh
RUN chown -R opam: . && chmod +x ./start-servers.sh

ENTRYPOINT ["/project/start-servers.sh", "_build/default/src/bin/tfb.exe"]
