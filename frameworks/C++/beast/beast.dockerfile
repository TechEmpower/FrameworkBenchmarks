FROM ubuntu:focal

# Install tools required for the project
RUN set -ex; \
    apt-get update \
    && apt-get install gcc -y \
    && apt-get install g++ -y \
    && apt-get install gdb -y \
    && apt-get install cmake -y \
    && apt-get install wget -y \
    && apt-get install libboost-all-dev -y \
    && apt-get install libjsoncpp-dev

# Copy the entire project and build it
COPY ./src /app/
WORKDIR /app/

RUN set -ex; \
    cd /app; \
    g++ -I /usr/include/jsoncpp app.cpp -g -o app -lpthread -ljsoncpp

ENTRYPOINT ["/app/app", "0.0.0.0", "8080", "/app/wwwroot", "100", "block"]

EXPOSE 8080
