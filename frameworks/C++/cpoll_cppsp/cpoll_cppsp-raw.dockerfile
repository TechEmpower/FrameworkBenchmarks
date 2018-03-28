FROM tfb/cpoll_cppsp-base:latest

CMD ./run_application /cpoll_cppsp/www -g g++-4.8 -m /forcedynamic.cppsm
