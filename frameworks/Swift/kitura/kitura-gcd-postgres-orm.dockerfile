FROM swift:4.1

ADD ./ /kitura
WORKDIR /kitura
RUN apt-get update -yqq && apt-get install -yqq libpq-dev
RUN swift build -c release -Xswiftc -DGCD_ASYNCH

EXPOSE 8080

CMD .build/release/TechEmpowerPostgresORM
