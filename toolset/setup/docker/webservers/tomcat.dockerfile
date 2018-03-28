FROM techempower/java:0.1

RUN mkdir /tomcat
WORKDIR /tomcat
RUN curl -s http://mirror.stjschools.org/public/apache/tomcat/tomcat-9/v9.0.6/bin/apache-tomcat-9.0.6.tar.gz | tar xz

# Remove the default app so that frameworks using Tomcat don't have to.
RUN rm -rf /tomcat/apache-tomcat-9.0.6/webapps/*

ENV CATALINA_HOME=/tomcat/apache-tomcat-9.0.6
