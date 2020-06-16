# Local development

During development it might be easier to start a PostgreSQL instance directly:

    sudo podman run --ulimit memlock=-1:-1 -it --rm=true --network host --memory-swappiness=0 --name HibernateTestingPGSQL -e POSTGRES_USER=benchmarkdbuser -e POSTGRES_PASSWORD=benchmarkdbpass -e POSTGRES_DB=hello_world -p 5432:5432 postgres:12

Then edit the `application.properties` resource, so to point to the database on localhost.

On first run make sure you set Hibernate to create the schema:

    quarkus.hibernate-orm.database.generation=drop-and-create

Build the application (might need to have installed the parent and dependencies first):

    mvn clean package

Run the application

    ./start-app.sh

If you just created the DB schema, you will need to create the test data. Hit this endpoint once:

    http://127.0.0.1:8080/createdata

Generate load on the application to test / profile it. I suggest to use `https://github.com/giltene/wrk2`.

Example run, assuming you have built wrk2 in `~/sources/wrk2` :

     ~/sources/wrk2/wrk -c 100 -d 60 -R 400 http://localhost:8080/db

The URL `http://localhost:8080/db` represents one specific benchmark; there are several more to try
but you will likely want to focus on them one at a time.


## Test URLs

### Plaintext Test

    http://localhost:8080/plaintext

### JSON Encoding Test

    http://localhost:8080/json

### Database Query Test

    http://localhost:8080/db

### Database Queries Test

    http://localhost:8080/queries?queries=5

### Database Update Test

    http://localhost:8080/updates?queries=5

### Template rendering Test

    http://localhost:8080/fortunes

## Full verification

Use the main Techempower script in the root to run all official verifications:

./tfb --type all --mode verify --test quarkus-hibernate

## Run the benchmark

./tfb --type all --mode benchmark --test quarkus-hibernate