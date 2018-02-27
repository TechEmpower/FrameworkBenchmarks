FROM tfb/undertow-base:latest
CMD java -jar target/hello-undertow.jar MONGODB_ASYNC
