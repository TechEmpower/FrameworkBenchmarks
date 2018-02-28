FROM tfb/nginx:latest

COPY ./ ./

RUN echo "worker_processes ${CPU_COUNT};" > nginx.conf
RUN echo "error_log /dev/null error;" >> nginx.conf
RUN echo "events {" >> nginx.conf
RUN echo "worker_connections 1024;" >> nginx.conf
RUN echo "}" >> nginx.conf
RUN echo "http {" >> nginx.conf
RUN echo "access_log off;" >> nginx.conf
RUN echo "include ${NGINX_HOME}/conf/mime.types;" >> nginx.conf
RUN echo "default_type application/octet-stream;" >> nginx.conf
RUN echo "sendfile on;" >> nginx.conf
RUN echo "upstream dart_cluster {" >> nginx.conf
RUN current=9001; end=$(($current+$CPU_COUNT)); while [ $current -lt $end ]; do echo "server 127.0.0.1:${current};" >> nginx.conf; current=$(($current+1)); done;
RUN echo "keepalive ${CPU_COUNT};" >> nginx.conf
RUN echo "}" >> nginx.conf
RUN echo "server {" >> nginx.conf
RUN echo "listen 8080;" >> nginx.conf
RUN echo "location / {" >> nginx.conf
RUN echo "proxy_pass http://dart_cluster;" >> nginx.conf
RUN echo "proxy_http_version 1.1;" >> nginx.conf
RUN echo "proxy_set_header Connection \"\";" >> nginx.conf
RUN echo "}" >> nginx.conf
RUN echo "}" >> nginx.conf
RUN echo "}" >> nginx.conf

RUN cat nginx.conf

CMD ["nginx", "-c", "/nginx.conf", "-g", "daemon off;"]