FROM tfb/nginx:latest

COPY ./ ./

RUN ./start-nginx.sh

CMD ["nginx", "-c", "/nginx.conf", "-g", "daemon off;"]
