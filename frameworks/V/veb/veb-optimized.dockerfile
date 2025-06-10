FROM thevlang/vlang:debian-dev
RUN apt update && \
    apt install -y libpq-dev default-libmysqlclient-dev

WORKDIR /app
COPY ./veb-pg/* ./
COPY run.sh ./
RUN v -prod -o veb .

EXPOSE 8080
CMD sh run.sh
