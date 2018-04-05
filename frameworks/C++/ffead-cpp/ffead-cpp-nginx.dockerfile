FROM techempower/ffead-cpp-base:0.1

WORKDIR $IROOT

RUN wget -q http://nginx.org/download/nginx-1.13.1.tar.gz
RUN tar xf nginx-1.13.1.tar.gz

WORKDIR $IROOT/nginx-1.13.1

RUN ./configure \
    --prefix=${IROOT}/nginxfc \
    --with-ld-opt="-lstdc++ -L${IROOT}/ffead-cpp-2.0/lib -L${IROOT}" \
    --add-module="${IROOT}/ffead-cpp-src/modules/nginx_mod_ffeadcpp" \
    --with-cc-opt="-I${IROOT}/ffead-cpp-2.0/include -I${IROOT}/include/libmongoc-1.0 -I${IROOT}/include/libbson-1.0 -w -fpermissive"
RUN make
RUN make install

RUN cp ${IROOT}/ffead-cpp-src/modules/nginx_mod_ffeadcpp/nginx.conf ${IROOT}/nginxfc/conf/
RUN sed -i 's|FFEAD_PATH|'${IROOT}/ffead-cpp-2.0'|g' ${IROOT}/nginxfc/conf/nginx.conf

ENV PATH=${IROOT}/nginxfc/sbin:${PATH}
