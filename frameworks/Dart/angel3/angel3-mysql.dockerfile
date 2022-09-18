FROM dart:latest

COPY ./orm-mysql/config /app/config
COPY ./orm-mysql/lib /app/lib
COPY ./orm-mysql/run /app/run
COPY ./orm-mysql/views /app/views
COPY ./orm-mysql/web /app/web
COPY ./orm-mysql/*.yaml /app/

WORKDIR /app
RUN dart pub upgrade

#RUN chmod -R 777 /app/run

# Optionally build generaed sources.
# RUN pub run build_runner build

# Set environment, start server
ENV ANGEL_ENV=production
EXPOSE 8080
CMD dart ./run/prod.dart -p 8080 -a 0.0.0.0 -j 100
#CMD dart ./run/dev.dart