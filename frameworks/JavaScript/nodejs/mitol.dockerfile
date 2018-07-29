FROM node:10.3.0

COPY ./ ./

RUN apt install -yqq wget
RUN npm install
RUN mkdir -p node_modules/mns & mkdir -p tmp
RUN wget -q https://github.com/Helidium/Mitol/archive/v0.0.1.tar.gz -P tmp
RUN tar -xzvf tmp/v0.0.1.tar.gz -C tmp
RUN make -C tmp/Mitol-0.0.1/node
RUN cp tmp/Mitol-0.0.1/node/dist/* node_modules/mns
