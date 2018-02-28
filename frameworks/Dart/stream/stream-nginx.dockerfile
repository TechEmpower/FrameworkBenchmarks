FROM tfb/nginx:latest

COPY ./ ./

CMD ["nginx", "-c", "/nginx.conf", "-g", "daemon off;"]
