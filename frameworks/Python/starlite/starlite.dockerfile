FROM python:3.11
WORKDIR /starlite/

RUN python -m venv /opt/venv
ENV PATH="/opt/venv/bin:$PATH"

COPY . .

RUN pip install --upgrade pip \
    && pip install cython==0.29.33 \
    && pip install -r /starlite/requirements.txt

EXPOSE 8080
CMD uvicorn app:app --host 0.0.0.0 --port 8080 --workers $(nproc) --log-level error --loop uvloop
