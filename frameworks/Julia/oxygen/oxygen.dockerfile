FROM julia:latest

WORKDIR /app
COPY ./src ./
RUN julia --project -e 'using Pkg; Pkg.instantiate()'

EXPOSE 8080
CMD julia -t 2 --project server.jl