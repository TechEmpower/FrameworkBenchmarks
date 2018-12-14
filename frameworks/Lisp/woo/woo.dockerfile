FROM fukamachi/roswell

RUN apt update -y
RUN apt install -y libev-dev

WORKDIR /woo
ADD  . .

RUN ["chmod", "+x", "./woo.ros"]

CMD ./woo.ros --worker $(nproc) --port 8080
