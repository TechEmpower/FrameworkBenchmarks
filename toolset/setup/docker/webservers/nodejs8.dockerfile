FROM techempower/base:0.2

ENV NODE_ENV=production

RUN curl -sL https://deb.nodesource.com/setup_8.x | bash -
RUN apt install -y nodejs
