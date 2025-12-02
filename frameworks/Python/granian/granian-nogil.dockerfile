FROM ghcr.io/astral-sh/uv:debian-slim

RUN uv python install 3.14t

ENV UV_PYTHON=3.14t
ENV PYTHON_GIL=0

ADD ./ /granian
WORKDIR /granian

RUN uv venv
RUN uv pip install -r requirements-nogil.txt

EXPOSE 8080

CMD uv run python run_nogil.py rsgi mt
