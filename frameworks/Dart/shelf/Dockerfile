FROM dart:stable AS build

WORKDIR /app
COPY pubspec.* ./
RUN dart pub get

COPY . .
RUN dart compile exe lib/server.dart -o lib/runner


FROM scratch
COPY --from=build /runtime/ /
COPY --from=build /app/lib/runner /app/runner

EXPOSE 8080
CMD ["/app/runner"]
