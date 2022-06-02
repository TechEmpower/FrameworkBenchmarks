## Build the docker image
```
docker build -t techempower/proxy .
```

## Run tfb postgres on port 5431
```
docker run -p 5431:5431 -d --rm --name tfb-database --network tfb techempower/postgres:latest postgres -p 5431
```

## Run proxy on port 5432
```
docker run -p 5432:5432 -d --rm --name tfb-proxy --network tfb techempower/proxy:latest 
```

## Run the tfb test with the proxy enabled
```
./tfb --test just --mode benchmark --type db --proxy on
```

## See the results from the proxy container while test is running
```
docker logs -f tfb-database
```