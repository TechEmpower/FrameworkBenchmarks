FROM python:2.7

COPY ./ ./
# Get v0.31 (no official releases that work 2015-06-25)

RUN git clone https://github.com/monkey/dudac.git
RUN cd dudac && git checkout 7c3d5b03b09fb4cb5f5e338fff72df2e25e95ef0 && \
    ./dudac -r && \
    ./dudac -s

EXPOSE 2001

CMD ["./dudac/dudac", "-w", "webservice", "-p", "2001"]
