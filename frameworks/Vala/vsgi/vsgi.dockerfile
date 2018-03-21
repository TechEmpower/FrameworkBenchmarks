FROM tfb/vala:latest

ADD ./ /vsgi_app
WORKDIR /vsgi_app

RUN meson --buildtype=release build
RUN ninja -C build

CMD bash run.sh
