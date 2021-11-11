FROM swift:4.1

ADD ./ /kitura
WORKDIR /kitura
RUN apt-get update -yqq && apt-get install -yqq libpq-dev
ENV KITURA_NIO=1
RUN swift build -c release

EXPOSE 8080

CMD .build/release/TechEmpowerPostgresORMCodable
