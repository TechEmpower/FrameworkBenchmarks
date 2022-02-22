FROM nginx:latest

RUN apt-get -y update
RUN apt-get -y upgrade
RUN apt-get -y install gfortran git sqlite3 make libsqlite3-dev libfcgi-dev spawn-fcgi

RUN git clone https://github.com/mapmeld/fortran-machine.git

WORKDIR /fortran-machine
COPY /src/fortran_fcgi.f90 fortran_fcgi.f90
COPY nginx.conf /etc/nginx/nginx.conf
COPY run.sh run.sh

RUN make

EXPOSE 8080

CMD bash run.sh

