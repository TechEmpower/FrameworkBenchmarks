CREATE USER benchmarkdbuser WITH PASSWORD 'benchmarkdbpass';

DROP DATABASE IF EXISTS hello_world;
CREATE DATABASE hello_world WITH ENCODING 'UTF8';

GRANT ALL PRIVILEGES ON DATABASE hello_world to benchmarkdbuser;
