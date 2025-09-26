FROM golang as build-env

WORKDIR /src/
ADD ./gin-gorm /src/

#- original submission
RUN go build -o app 
#RUN go build -tags=jsoniter -o app - tryed this but slower on my pc

FROM gcr.io/distroless/base:debug

ENV GIN_MODE=release

COPY --from=build-env /src/app /app
ENTRYPOINT ["/app"]