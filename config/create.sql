# create benchmark user
GRANT ALL ON *.* TO 'benchmarkdbuser'@'%' IDENTIFIED BY 'benchmarkdbpass';

# modified from SO answer http://stackoverflow.com/questions/5125096/for-loop-in-mysql
DROP DATABASE IF EXISTS hello_world;
CREATE DATABASE hello_world;
USE hello_world;

DROP TABLE IF EXISTS World;
CREATE TABLE  World (
  id int(10) unsigned NOT NULL auto_increment,
  randomNumber int NOT NULL default 0,
  PRIMARY KEY  (id)
)
ENGINE=INNODB;

DROP PROCEDURE IF EXISTS load_data;

DELIMITER #
CREATE PROCEDURE load_data()
BEGIN

declare v_max int unsigned default 10000;
declare v_counter int unsigned default 0;

  TRUNCATE TABLE World;
  START TRANSACTION;
  while v_counter < v_max do
    INSERT INTO World (randomNumber) VALUES ( floor(0 + (rand() * 10000)) );
    SET v_counter=v_counter+1;
  end while;
  commit;
END #

DELIMITER ;

CALL load_data();
