FROM techempower/base:0.2

RUN add-apt-repository -y ppa:openjdk-r/ppa
RUN apt-get update
RUN apt-get install -qqy -o Dpkg::Options::="--force-confdef" -o Dpkg::Options::="--force-confold" \
    openjdk-8-jdk

# https://bugs.launchpad.net/ubuntu/+source/ca-certificates-java/+bug/1396760
RUN /var/lib/dpkg/info/ca-certificates-java.postinst configure

ENV JAVA_HOME=/usr/lib/jvm/java-8-openjdk-amd64
ENV PATH="/usr/lib/jvm/java-8-openjdk-amd64/bin:${PATH}"
