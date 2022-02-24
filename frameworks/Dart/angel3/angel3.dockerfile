FROM dart:latest

COPY ./orm/config /app/config
COPY ./orm/lib /app/lib
COPY ./orm/run /app/run
COPY ./orm/views /app/views
COPY ./orm/web /app/web
COPY ./orm/*.yaml /app/

WORKDIR /app
RUN dart pub upgrade

#RUN chmod -R 777 /app/run

# Optionally build generaed sources.
# RUN pub run build_runner build

# Set environment, start server
ENV ANGEL_ENV=production
EXPOSE 8080
CMD dart ./run/prod.dart -p 8080 -a 0.0.0.0 -j 50
