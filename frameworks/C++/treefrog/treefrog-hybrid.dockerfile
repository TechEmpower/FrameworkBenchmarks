FROM techempower/treefrog-base:0.1

RUN sed -i 's|DriverType=.*|DriverType=QMYSQL|g' config/database.ini
RUN sed -i 's|MultiProcessingModule=.*|MultiProcessingModule=hybrid|g' config/application.ini

# 1. Generate Makefile
RUN qmake -r CONFIG+=release

# 2. Compile applicaton
RUN make

# 3. Start TreeFrog
CMD treefrog /
