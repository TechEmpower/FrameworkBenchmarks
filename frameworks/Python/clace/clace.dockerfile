FROM python:3.11
WORKDIR /clace/

RUN curl -L https://clace.io/install.sh | bash
ENV CL_HOME="/root/clhome"
ENV PATH="/root/clhome/bin:$PATH"

COPY . .

EXPOSE 8080
CMD /clace/run.sh
