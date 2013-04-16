DROP TABLE IF EXISTS Fortune;
CREATE TABLE  Fortune (
  id int(10) unsigned NOT NULL auto_increment,
  message varchar(2048) CHARACTER SET 'utf8' NOT NULL,
  PRIMARY KEY  (id)
)
ENGINE=INNODB;

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