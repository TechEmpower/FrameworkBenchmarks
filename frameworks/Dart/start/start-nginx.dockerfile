FROM tfb/nginx:latest

COPY ./ ./

CMD ["./start-nginx.sh"]
