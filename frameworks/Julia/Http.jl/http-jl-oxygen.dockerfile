FROM julia:1.9.3

ENV DEBIAN_FRONTEND noninteractive

RUN apt-get update -yqq && apt-get install -y
RUN apt-get update -yqq && apt-get install -y wget

COPY ./Project.toml ./Manifest.toml ./

# This step takes a lot of time, let's aggressively cache it
RUN julia --project=. -e 'import Pkg; Pkg.instantiate();'

# Files
COPY ./ ./

RUN chmod +x run.sh

EXPOSE 8080

CMD julia --threads=auto --project=. oxygen.jl
