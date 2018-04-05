FROM techempower/h2o:0.1

ADD ./h2o.conf /h2o_mruby/

CMD "${H2O_HOME}/bin/h2o" -c "/h2o_mruby/h2o.conf"
