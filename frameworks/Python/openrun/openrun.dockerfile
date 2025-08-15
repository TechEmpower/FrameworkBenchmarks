FROM python:3.11
WORKDIR /openrun/

RUN curl -L https://openrun.dev/install.sh | bash
ENV OPENRUN_HOME="/root/openrun"
ENV PATH="/root/openrun/bin:$PATH"

COPY . .

EXPOSE 8080
CMD /openrun/run.sh
