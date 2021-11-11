FROM sumeetchhetri/ffead-cpp-sql-raw-async-pool-profiled-base:6.0

ENV IROOT=/installs

WORKDIR /

RUN sed -i 's|router="TeBkUmLpqAsyncRouter"|router="TeBkUmLpqAsyncRouter" properties="app.prop"|g' ${IROOT}/ffead-cpp-6.0-sql/web/te-benchmark-um-pq-async/config/application.xml
RUN echo "dbpoolsize=4" > ${IROOT}/ffead-cpp-6.0-sql/web/te-benchmark-um-pq-async/config/app.prop

CMD ./run_ffead.sh ffead-cpp-6.0-sql emb postgresql-raw-async memory