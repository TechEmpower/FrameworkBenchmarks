FROM dart:stable AS build

WORKDIR /app
COPY pubspec.* ./
RUN dart pub get

COPY . .
RUN dart compile exe lib/server.dart -o lib/server


FROM scratch
COPY --from=build /runtime/ /
COPY --from=build /app/lib/server /app/lib/

EXPOSE 8080
CMD ["/app/lib/server"]
