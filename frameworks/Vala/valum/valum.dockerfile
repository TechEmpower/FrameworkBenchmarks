FROM tfb/vala:latest

ADD ./ /valum_app
WORKDIR /valum_app

RUN meson --buildtype=release build
RUN ninja -C build

CMD bash run.sh
