FROM ubuntu:22.04

WORKDIR /app

# Build ki lang from source
RUN apt-get update
RUN apt-get install sudo -y
RUN apt-get install git -y
RUN apt-get install build-essential -y
RUN apt-get install curl -y
RUN apt-get install libxml2-dev -y
RUN apt-get install libc6 -y
RUN apt-get install zlib1g-dev -y

RUN echo "## Download"

RUN curl -s https://ki-lang.dev/dist/install.sh | bash -s techempower

# Copy app code
COPY ./main.ki /app
# Build app
RUN ki build main.ki -o ./server --clean --optimize --static -v

EXPOSE 8080

CMD ./server
