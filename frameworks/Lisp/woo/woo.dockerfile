FROM fukamachi/roswell

WORKDIR /woo
ADD  . .

RUN ros install woo clack alexandria optima jonathan

CMD ["clackup", "woo.lisp"]
