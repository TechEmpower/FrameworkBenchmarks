FROM techempower/java:0.1

RUN add-apt-repository -y ppa:openjdk-r/ppa
RUN apt-get update
RUN apt-get install -qqy -o Dpkg::Options::="--force-confdef" -o Dpkg::Options::="--force-confold" \
    ant