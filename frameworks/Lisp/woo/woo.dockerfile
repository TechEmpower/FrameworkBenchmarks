FROM fukamachi/roswell

WORKDIR /woo
ADD  . .

RUN apt install -y libev-dev

RUN ["chmod", "+x", "./woo.ros"]

CMD ./woo.ros --worker $(nproc) --port 8080
