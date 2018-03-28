FROM techempower/vala:0.1

ADD ./ /vsgi_app
WORKDIR /vsgi_app

RUN meson --buildtype=release build
RUN ninja -C build

CMD bash run.sh
