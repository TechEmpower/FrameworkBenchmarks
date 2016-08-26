/*
docker run --name postgres \
      -p 5432:5432 \
      -e POSTGRES_USER=benchmarkdbuser \
      -e POSTGRES_PASSWORD=benchmarkdbpass \
      -e POSTGRES_DB=hello_world \
      -d postgres:9.5.3
*/

CREATE SCHEMA hello_world;

-- ----------------------------
-- World Table
-- ----------------------------

CREATE TABLE hello_world.World (
  id SERIAL NOT NULL ,
  randomnumber INT NOT NULL,
  PRIMARY KEY ( id )
);

INSERT INTO hello_world.World
  SELECT id, CAST( random() * 10000 AS int )
  FROM generate_series(1, 10000) AS id

SELECT * FROM hello_world.World;


-- ----------------------------
-- Fortunes Table
-- ----------------------------

CREATE TABLE hello_world.Fortune (
  id SERIAL NOT NULL,
  message VARCHAR(255),
  PRIMARY KEY ( id )
);

INSERT INTO hello_world.Fortune (message) VALUES ('fortune: No such file or directory');
INSERT INTO hello_world.Fortune (message) VALUES ('A computer scientist is someone who fixes things that arent broken.');
INSERT INTO hello_world.Fortune (message) VALUES ('After enough decimal places, nobody gives a damn.');
INSERT INTO hello_world.Fortune (message) VALUES ('A bad random number generator: 1, 1, 1, 1, 1, 4.33e+67, 1, 1, 1');
INSERT INTO hello_world.Fortune (message) VALUES ('A computer program does what you tell it to do, not what you want it to do.');
INSERT INTO hello_world.Fortune (message) VALUES ('Emacs is a nice operating system, but I prefer UNIX. - Tom Christaensen');
INSERT INTO hello_world.Fortune (message) VALUES ('Any program that runs right is obsolete.');
INSERT INTO hello_world.Fortune (message) VALUES ('A list is only as strong as its weakest link. - Donald Knuth');
INSERT INTO hello_world.Fortune (message) VALUES ('Feature: A bug with seniority.');
INSERT INTO hello_world.Fortune (message) VALUES ('Computers make very fast, very accurate mistakes.');
INSERT INTO hello_world.Fortune (message) VALUES ('<script>alert("This should not be displayed in a browser alert box.");</script>');
INSERT INTO hello_world.Fortune (message) VALUES ('フレームワークのベンチマーク');

SELECT * FROM hello_world.Fortune;

