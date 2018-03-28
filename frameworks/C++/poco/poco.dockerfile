FROM techempower/gcc-4.8:0.1

ENV IROOT=/install
ENV VERSION=1.6.1
ENV POCO_HOME=$IROOT/poco_$VERSION

RUN mkdir install

WORKDIR /install

RUN wget -q http://pocoproject.org/releases/poco-$VERSION/poco-$VERSION-all.tar.gz
RUN tar xf poco-$VERSION-all.tar.gz

RUN cp -R poco-$VERSION-all/ $POCO_HOME

WORKDIR $POCO_HOME

RUN ./configure --no-tests --no-samples
RUN make --quiet PageCompiler-libexec XML-libexec JSON-libexec

ENV LD_LIBRARY_PATH=$POCO_HOME/lib/Linux/x86_64

COPY ./benchmark.cpp ./

RUN  g++-4.8 -O3 -DNDEBUG -std=c++0x -o poco benchmark.cpp -I$POCO_HOME/Foundation/include -I$POCO_HOME/Util/include -I$POCO_HOME/Net/include -L$POCO_HOME/lib/Linux/x86_64 -lPocoNet -lPocoUtil -lPocoFoundation -lPocoXML -lPocoJSON

CMD ./poco 8080 $CPU_COUNT
