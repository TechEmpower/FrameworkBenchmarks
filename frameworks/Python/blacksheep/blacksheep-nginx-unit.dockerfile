FROM unit:python3.13-slim

WORKDIR /blacksheep

COPY ./ /blacksheep

RUN pip3 install -U pip -q
RUN pip3 install Cython==3.0.12 -q
RUN pip3 install -r /blacksheep/requirements.txt -q
RUN pip3 install -r /blacksheep/requirements-uvicorn.txt -q

ENV PGSSLMODE=disable
RUN CORE_COUNT=$(nproc) && \
    sed -i "s|\"processes\": [0-9]*|\"processes\": $CORE_COUNT|g" /blacksheep/unit-config.json

RUN chmod +x start-unit.sh
ENTRYPOINT []
EXPOSE 8080

# CMD ["unitd", "--no-daemon", "--control", "unix:/var/run/control.unit.sock"]
CMD ["./start-unit.sh"]