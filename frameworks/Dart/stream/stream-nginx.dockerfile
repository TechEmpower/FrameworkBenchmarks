FROM tfb/nginx:latest

COPY ./ ./

RUN chmod a+rwx nginx-conf.sh
RUN ./nginx-conf.sh

CMD ["nginx", "-c", "/nginx.conf", "-g", "daemon off;"]
