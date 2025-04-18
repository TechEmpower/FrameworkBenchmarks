FROM unit:python3.13-slim

WORKDIR /blacksheep

COPY ./ /blacksheep

RUN pip3 install -U pip
RUN pip3 install Cython==3.0.12
RUN pip3 install -r /blacksheep/requirements.txt
RUN pip3 install -r /blacksheep/requirements-uvicorn.txt

RUN chmod +x start-unit.sh

ENV PGSSLMODE=disable

EXPOSE 8080
CMD ["./start-unit.sh"]
