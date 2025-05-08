FROM unit:python3.13-slim

WORKDIR /blacksheep

COPY ./ /blacksheep

RUN pip3 install -U pip -q
RUN pip3 install Cython==3.0.12 -q
RUN pip3 install -r /blacksheep/requirements.txt -q
RUN pip3 install -r /blacksheep/requirements-uvicorn.txt -q

ENV PGSSLMODE=disable
RUN CORE_COUNT=$(nproc) && \
    MAX_PROCESSES=$((CORE_COUNT <= 4 ? CORE_COUNT : CORE_COUNT / 2)) && \
    SPARE_PROCESSES=$((MAX_PROCESSES / 4 > 0 ? MAX_PROCESSES / 4 : 1)) && \
    sed -i "s|\"max\": [0-9]*|\"max\": $MAX_PROCESSES|g" /blacksheep/unit-config.json && \
    sed -i "s|\"spare\": [0-9]*|\"spare\": $SPARE_PROCESSES|g" /blacksheep/unit-config.json && \
    sed -i "s|\"idle_timeout\": [0-9]*|\"idle_timeout\": 3600|g" /blacksheep/unit-config.json

RUN chmod +x start-unit.sh
ENTRYPOINT []
EXPOSE 8080

# CMD ["unitd", "--no-daemon", "--control", "unix:/var/run/control.unit.sock"]
CMD ["./start-unit.sh"]