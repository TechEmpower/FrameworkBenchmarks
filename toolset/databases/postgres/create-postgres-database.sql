CREATE USER benchmarkdbuser WITH PASSWORD 'benchmarkdbpass';

ALTER USER benchmarkdbuser WITH SUPERUSER;

CREATE DATABASE hello_world WITH TEMPLATE = template0 ENCODING 'UTF8';
