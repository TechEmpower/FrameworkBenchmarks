FROM sumeetchhetri/ffead-cpp-sql-raw-async-pool-profiled-base:6.0

ENV IROOT=/installs

WORKDIR /

RUN sed -i 's|router="TeBkUmLpqAsyncRouter"|router="TeBkUmLpqAsyncRouter" properties="app.prop"|g' ${IROOT}/ffead-cpp-6.0-sql/web/t4/config/application.xml
RUN echo "dbpoolsize=4" > ${IROOT}/ffead-cpp-6.0-sql/web/t4/config/app.prop
RUN sed -i 's|router="TeBkUmLpqQwAsyncRouter"|router="TeBkUmLpqQwAsyncRouter" properties="app.prop"|g' ${IROOT}/ffead-cpp-6.0-sql/web/t5/config/application.xml
RUN echo "dbpoolsize=4" > ${IROOT}/ffead-cpp-6.0-sql/web/t5/config/app.prop

CMD ./run_ffead.sh ffead-cpp-6.0-sql emb postgresql-raw-async memory