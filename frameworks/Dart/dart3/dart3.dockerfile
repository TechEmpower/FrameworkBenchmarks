FROM dart:3.4.4 AS build

COPY . /app
WORKDIR /app
RUN dart compile exe ./bin/server.dart -o ./server

FROM scratch
COPY --from=build /runtime/ /
COPY --from=build /app/ /bin

EXPOSE 8080
CMD ["server"]