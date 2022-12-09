FROM nginx:mainline

ADD ./ ./

EXPOSE 8080

CMD ["nginx", "-c", "/njs/nginx.conf"]
