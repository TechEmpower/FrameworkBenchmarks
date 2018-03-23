FROM ubuntu:16.04

# One -q produces output suitable for logging (mostly hides
# progress indicators)
RUN apt-get -yq update

RUN apt-get -y install -o Dpkg::Options::="--force-confdef" -o Dpkg::Options::="--force-confold" \
    build-essential git libev-dev libpq-dev libreadline6-dev curl

##############################
# wrk
##############################
RUN curl -sL -o wrk-4.0.1.tar.gz https://github.com/wg/wrk/archive/4.0.1.tar.gz
RUN tar xzf wrk-4.0.1.tar.gz
RUN cd wrk-4.0.1 && make
RUN cp wrk-4.0.1/wrk /usr/local/bin

#############################
# pipeline.lua
#############################
RUN echo "init = function(args)" >> pipeline.lua; \
    echo "  local r = {}" >> pipeline.lua; \
    echo "  local depth = tonumber(args[1]) or 1" >> pipeline.lua; \
    echo "  for i=1,depth do" >> pipeline.lua; \
    echo "    r[i] = wrk.format()" >> pipeline.lua; \
    echo "  end" >> pipeline.lua; \
    echo "  req = table.concat(r)" >> pipeline.lua; \
    echo "end" >> pipeline.lua; \
    echo "request = function()" >> pipeline.lua; \
    echo "  return req" >> pipeline.lua; \
    echo "end" >> pipeline.lua