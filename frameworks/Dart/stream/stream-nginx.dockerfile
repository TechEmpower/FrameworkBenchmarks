FROM tfb/nginx:latest

COPY ./ ./

CMD ["./stream-nginx.sh"]
