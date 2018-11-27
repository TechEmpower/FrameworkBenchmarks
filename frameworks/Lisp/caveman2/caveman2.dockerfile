FROM fukamachi/roswell

WORKDIR /woo
ADD  . .

RUN ros install clack

CMD APP_ENV=development clackup --server :fcgi --port 8080 app.lisp
