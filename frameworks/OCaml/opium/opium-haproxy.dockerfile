FROM ocurrent/opam:fedora-32-ocaml-4.11

ENV DIR web
# https://caml.inria.fr/pub/docs/manual-ocaml/libref/Gc.html
# https://linux.die.net/man/1/ocamlrun
# https://blog.janestreet.com/memory-allocator-showdown/
ENV OCAMLRUNPARAM a=2,o=240

RUN sudo dnf install --assumeyes postgresql-devel libev-devel libffi-devel

WORKDIR /${DIR}

COPY src/opi.opam src/Makefile ./

RUN make install-ci

ENV APP_INSTANCES 1

COPY ./src ./

RUN sudo chown -R opam: . && make build

# try to keep everything above here in sync with other dockerfiles in project for more efficent use of docker build cache
RUN sudo dnf install --assumeyes haproxy
COPY haproxy.cfg /etc/haproxy/haproxy.cfg
COPY start-servers.sh ./start-servers.sh
RUN sudo chown -R opam: /${DIR} && chmod +x ./start-servers.sh

EXPOSE 8080

ENTRYPOINT ./start-servers.sh && sudo /usr/sbin/haproxy -W -f /etc/haproxy/haproxy.cfg -p /run/haproxy.pid
