FROM sumeetchhetri/ffead-cpp-base:6.0

#seastar needs ubuntu 20 and boost >= 1.66
WORKDIR ${IROOT}

ENV DEBIAN_FRONTEND noninteractive

RUN rm -f /usr/local/lib/libffead-* /usr/local/lib/libte_benc* /usr/local/lib/libinter.so /usr/local/lib/libdinter.so && \
	ln -s ${IROOT}/ffead-cpp-6.0/lib/libte-benchmark-um.so /usr/local/lib/libte-benchmark-um.so && \
	ln -s ${IROOT}/ffead-cpp-6.0/lib/libte-benchmark-um-pq.so /usr/local/lib/libte-benchmark-um-pq.so && \
	ln -s ${IROOT}/ffead-cpp-6.0/lib/libte-benchmark-um-mgr.so /usr/local/lib/libte-benchmark-um-mgr.so && \
	ln -s ${IROOT}/ffead-cpp-6.0/lib/libte-benchmark-um-pq-async.so /usr/local/lib/libte-benchmark-um-pq-async.so && \
	ln -s ${IROOT}/ffead-cpp-6.0/lib/libffead-modules.so /usr/local/lib/libffead-modules.so && \
	ln -s ${IROOT}/ffead-cpp-6.0/lib/libffead-framework.so /usr/local/lib/libffead-framework.so && \
	ln -s ${IROOT}/ffead-cpp-6.0/lib/libinter.so /usr/local/lib/libinter.so && \
	ln -s ${IROOT}/ffead-cpp-6.0/lib/libdinter.so /usr/local/lib/libdinter.so && \
	ldconfig

ENV FFEAD_CPP_PATH=${IROOT}/ffead-cpp-6.0
ENV LD_LIBRARY_PATH=${IROOT}/:${IROOT}/lib:${FFEAD_CPP_PATH}/lib:/usr/local/lib:$LD_LIBRARY_PATH

#seastar need hwloc 2
RUN cd /tmp && wget -q https://github.com/open-mpi/hwloc/releases/download/hwloc-2.1.0/hwloc-2.1.0.tar.gz && \
	tar xvf hwloc-2.1.0.tar.gz && cd hwloc-2.1.0 && ./configure --prefix=/usr/local/ && make install
RUN rm -rf /tmp/hwloc-2.1.0

RUN apt update -y && apt install -y pkg-config liblzma-dev libunistring-dev libudev-dev bridge-utils \
	net-tools iproute2 kmod sudo qemu-kvm libvirt-clients libvirt-daemon-system
	
RUN sudo adduser $(whoami) libvirt
RUN sudo adduser $(whoami) kvm
RUN sudo adduser $(whoami) libvirt-qemu
RUN sudo adduser $(whoami) libvirt-dnsmasq
#RUN sudo chown $(whoami) /dev/kvm
#RUN sudo chmod 777 /dev/kvm

#seastar needs gcc-10
RUN git clone https://github.com/sumeetchhetri/seastar && cd seastar && git checkout for_ffead
RUN cd seastar && chmod +x *.sh && apt update -y && ./install-dependencies.sh && apt remove -y libfmt-dev && \
	./configure.py --mode=release --cook fmt && ./configure.py --mode=release --prefix=/usr/local
RUN cd seastar && ninja -C build/release install && cp build/release/_cooking/installed/lib/libfmt.a /usr/local/lib/ && \
	cp -rf build/release/_cooking/installed/include/fmt /usr/local/include/ && cp apps/lib/stop_signal.hh /${IROOT}/lang-server-backends/c++/seastar && \
	cd ${IROOT} && rm -rf ${IROOT}/seastar && mkdir -p ${IROOT}/seastar/build/release/_cooking/installed/lib/ && \
	cp /usr/local/lib/libfmt.a ${IROOT}/seastar/build/release/_cooking/installed/lib/

WORKDIR ${IROOT}/lang-server-backends/c++/seastar

#RUN g++ -g SeastarFfeadCpp.cpp -I/home/mavuser/ffead-cpp-6.0/include/ -I/usr/include/libmongoc-1.0 \
#	-I/usr/include/libbson-1.0 -I. -I/usr/local/include $(pkg-config --libs --cflags --static seastar) -lffead-framework \
#	-lffead-modules -o ffead-cpp-seastar

RUN g++ SeastarFfeadCpp.cpp -O3 -I. $(pkg-config --libs --cflags --static seastar) -lffead-framework -o ffead-cpp-seastar

RUN chmod +x run.sh

WORKDIR /

CMD ./run_ffead.sh ffead-cpp-6.0 seastar

