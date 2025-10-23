FROM alpine:latest
RUN apk update && apk upgrade
RUN apk add gcc musl-dev ldc dub llvm-libunwind-static openssl-libs-static git make
RUN ln -fs /usr/share/zoneinfo/Europe/Rome /etc/localtime
RUN apk add tzdata
RUN apk cache clean

RUN adduser -D -S www-data

WORKDIR /app
COPY . .
RUN dub build --build=release-nobounds

EXPOSE 8080
USER www-data
CMD ["./serverino-benchmark"]
