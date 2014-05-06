
DROP KEYSPACE tfb;

CREATE KEYSPACE tfb WITH replication = {'class': 'SimpleStrategy', 'replication_factor': 1 };
USE tfb;

CREATE TABLE  World (
  id int,
  randomNumber int,
  PRIMARY KEY (id)
);
