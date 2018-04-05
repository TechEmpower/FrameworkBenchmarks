FROM techempower/php7:0.1

RUN apt-get install -y apt-transport-https
RUN apt-key adv --recv-keys --keyserver hkp://keyserver.ubuntu.com:80 0xB4112585D386EB94
RUN add-apt-repository https://dl.hhvm.com/ubuntu
RUN apt-get update
RUN apt-get install -y hhvm
