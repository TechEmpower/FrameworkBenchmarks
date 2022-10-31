FROM thevlang/vlang:alpine-dev

WORKDIR /app

COPY . .

RUN v up && v -prod app-mysql-orm.v