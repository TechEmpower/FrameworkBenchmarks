FROM tfb/h2o:latest

ADD ./h2o.conf /h2o_mruby/

CMD "${H2O_HOME}/bin/h2o" -c "/h2o_mruby/h2o.conf"
