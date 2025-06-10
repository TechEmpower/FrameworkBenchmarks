FROM thevlang/vlang:debian-dev

WORKDIR /app
COPY ./main.v run.sh ./

RUN v -prod -o picoev .

EXPOSE 8080
CMD ./picoev
