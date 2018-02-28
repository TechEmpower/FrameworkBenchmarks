FROM tfb/nginx:latest

COPY ./ ./

RUN ./nginx-conf.sh

CMD ["nginx", "-c", "/nginx.conf", "-g", "daemon off;"]
