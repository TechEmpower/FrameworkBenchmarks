FROM rstudio/plumber

RUN apt-get update

ADD ./ /plumber

WORKDIR /plumber

EXPOSE 8080

CMD ["deploy_plumber.R"]
