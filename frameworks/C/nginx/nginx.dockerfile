FROM nginx:mainline

ADD ./ ./

EXPOSE 8080

CMD ["nginx", "-c", "/nginx.conf"]