FROM tfb/java8:latest
ENV LEIN_HOME=/lein
RUN mkdir ${LEIN_HOME}
RUN mkdir ${LEIN_HOME}/bin
RUN wget -nv -O ${LEIN_HOME}/bin/lein https://raw.github.com/technomancy/leiningen/stable/bin/lein
RUN chmod +x ${LEIN_HOME}/bin/lein
ENV PATH=${LEIN_HOME}/bin:${PATH}
