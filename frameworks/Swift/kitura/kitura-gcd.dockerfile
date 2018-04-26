FROM swift:4.1

ADD ./ /kitura
WORKDIR /kitura
RUN apt update -yqq && apt install -yqq libpq-dev
RUN swift build -c release -Xswiftc -DGCD_ASYNCH
CMD .build/release/TechEmpowerKuery
