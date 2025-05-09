FROM unit:python3.13-slim

WORKDIR /blacksheep

COPY ./ /blacksheep

RUN pip3 install -U pip -q
RUN pip3 install Cython==3.0.12 -q
RUN pip3 install -r /blacksheep/requirements.txt -q
RUN pip3 install -r /blacksheep/requirements-uvicorn.txt -q

ENV PGSSLMODE=disable
RUN CORE_COUNT=$(nproc) && \
    PROCESSES=$(($CORE_COUNT <= 4 ? $CORE_COUNT : $CORE_COUNT / 4)) && \
    PROCESSES=$(($PROCESSES < 4 ? 4 : $PROCESSES > 16 ? 16 : $PROCESSES)) && \
    THREADS=$(($CORE_COUNT / $PROCESSES)) && \
    THREADS=$(($THREADS < 10 ? 10 : $THREADS)) && \
    MAX_POOL_SIZE=$((2000 / $PROCESSES)) && \
    MIN_POOL_SIZE=$(($MAX_POOL_SIZE / 2)) && \
    sed -i "s|\"processes\": [0-9]*.*\"threads\": [0-9]*|\"processes\": $PROCESSES, \"threads\": $THREADS|g" /blacksheep/unit-config.json && \
    echo "export MAX_POOL_SIZE=$MAX_POOL_SIZE" >> /blacksheep/env.sh && \
    echo "export MIN_POOL_SIZE=$MIN_POOL_SIZE" >> /blacksheep/env.sh

RUN chmod +x start-unit.sh
RUN chmod +x env.sh
ENTRYPOINT []
EXPOSE 8080

# CMD ["unitd", "--no-daemon", "--control", "unix:/var/run/control.unit.sock"]
# CMD ["./start-unit.sh"]
CMD ["bash", "-c", "source /blacksheep/env.sh && ./start-unit.sh"]