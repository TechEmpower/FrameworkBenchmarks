FROM tfb/treefrog-base:latest

RUN sed -i 's|DriverType=.*|DriverType=QMYSQL|g' config/database.ini
RUN sed -i 's|MultiProcessingModule=.*|MultiProcessingModule=thread|g' config/application.ini

# 1. Generate Makefile
RUN qmake -r CONFIG+=release

# 2. Compile applicaton
RUN make

# 3. Start TreeFrog
CMD treefrog /
