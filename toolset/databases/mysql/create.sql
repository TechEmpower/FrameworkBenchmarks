# To maintain consistency across servers and fix a problem with the jdbc per
# http://stackoverflow.com/questions/37719818/the-server-time-zone-value-aest-is-unrecognized-or-represents-more-than-one-ti
SET GLOBAL time_zone = '+00:00';

CREATE USER 'benchmarkdbuser'@'%' IDENTIFIED WITH mysql_native_password BY 'benchmarkdbpass';
CREATE USER 'benchmarkdbuser'@'localhost' IDENTIFIED WITH mysql_native_password BY 'benchmarkdbpass';

-- GitHub Actions/CI run the database server on the same system as the benchmarks.
-- Because we setup MySQL with the skip-name-resolve option, the IP address 127.0.0.1 might not be resolved to localhost
-- anymore. This does not seem to matter, as long as Unix sockets are being used (e.g. when setting up the docker image),
-- because the host is set to be localhost implicitly, but it matters for local TCP connections.
CREATE USER 'benchmarkdbuser'@'127.0.0.1' IDENTIFIED WITH mysql_native_password BY 'benchmarkdbpass';

# modified from SO answer http://stackoverflow.com/questions/5125096/for-loop-in-mysql
CREATE DATABASE hello_world;
USE hello_world;

CREATE TABLE  world (
  id int(10) unsigned NOT NULL auto_increment,
  randomNumber int NOT NULL default 0,
  PRIMARY KEY  (id)
)
ENGINE=INNODB;
GRANT ALL PRIVILEGES ON hello_world.world TO 'benchmarkdbuser'@'%';
GRANT ALL PRIVILEGES ON hello_world.world TO 'benchmarkdbuser'@'localhost';
GRANT ALL PRIVILEGES ON hello_world.world TO 'benchmarkdbuser'@'127.0.0.1';

DELIMITER #
CREATE PROCEDURE load_data()
BEGIN

declare v_max int unsigned default 10000;
declare v_counter int unsigned default 0;

  TRUNCATE TABLE world;
  START TRANSACTION;
  while v_counter < v_max do
    INSERT INTO world (randomNumber) VALUES ( least(floor(1 + (rand() * 10000)), 10000) );
    SET v_counter=v_counter+1;
  end while;
  commit;
END
#

DELIMITER ;

CALL load_data();

CREATE TABLE  fortune (
  id int(10) unsigned NOT NULL auto_increment,
  message varchar(2048) CHARACTER SET 'utf8' NOT NULL,
  PRIMARY KEY  (id)
)
ENGINE=INNODB;
GRANT ALL PRIVILEGES ON hello_world.fortune TO 'benchmarkdbuser'@'%';
GRANT ALL PRIVILEGES ON hello_world.fortune TO 'benchmarkdbuser'@'localhost';
GRANT ALL PRIVILEGES ON hello_world.fortune TO 'benchmarkdbuser'@'127.0.0.1';

INSERT INTO fortune (message) VALUES ('fortune: No such file or directory');
INSERT INTO fortune (message) VALUES ('A computer scientist is someone who fixes things that aren''t broken.');
INSERT INTO fortune (message) VALUES ('After enough decimal places, nobody gives a damn.');
INSERT INTO fortune (message) VALUES ('A bad random number generator: 1, 1, 1, 1, 1, 4.33e+67, 1, 1, 1');
INSERT INTO fortune (message) VALUES ('A computer program does what you tell it to do, not what you want it to do.');
INSERT INTO fortune (message) VALUES ('Emacs is a nice operating system, but I prefer UNIX. — Tom Christaensen');
INSERT INTO fortune (message) VALUES ('Any program that runs right is obsolete.');
INSERT INTO fortune (message) VALUES ('A list is only as strong as its weakest link. — Donald Knuth');
INSERT INTO fortune (message) VALUES ('Feature: A bug with seniority.');
INSERT INTO fortune (message) VALUES ('Computers make very fast, very accurate mistakes.');
INSERT INTO fortune (message) VALUES ('<script>alert("This should not be displayed in a browser alert box.");</script>');
INSERT INTO fortune (message) VALUES ('フレームワークのベンチマーク');
