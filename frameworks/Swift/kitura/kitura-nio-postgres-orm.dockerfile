FROM swift:4.1

ADD ./ /kitura
WORKDIR /kitura
RUN apt-get update -yqq && apt-get install -yqq libpq-dev
ENV KITURA_NIO=1
RUN swift build -c release
CMD .build/release/TechEmpowerPostgresORM
