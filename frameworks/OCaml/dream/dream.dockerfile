FROM ocaml/opam:debian-ocaml-5.1
COPY ./dream_test/ /app
WORKDIR /app
USER root
RUN apt install -y libgmp-dev libev-dev pkg-config libssl-dev
RUN opam install --yes --deps-only .
RUN opam install dune
RUN eval $(opam env)
RUN opam exec -- dune build --profile release
CMD [ "./_build/default/bin/main.exe" ]
EXPOSE 8080

