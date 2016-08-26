/*
docker run --name mysql \
      -p 3306:3306 \
      -e MYSQL_ROOT_PASSWORD=password \
      -e MYSQL_USER=benchmarkdbuser \
      -e MYSQL_PASSWORD=benchmarkdbpass \
      -e MYSQL_DATABASE=hello_world \
      -d mysql:5.7 \
          --character-set-server=utf8mb4 \
          --collation-server=utf8mb4_unicode_ci
*/

-- ----------------------------
-- World Table
-- ----------------------------


CREATE TABLE hello_world.World (
  id INT NOT NULL AUTO_INCREMENT,
  randomnumber INT NOT NULL,
  PRIMARY KEY ( id )
);

CREATE PROCEDURE populate_world(IN NumRows INT, IN MinVal INT, IN MaxVal INT)
BEGIN
    DECLARE i INT;
    SET i = 1;
    START TRANSACTION;
    WHILE i <= NumRows DO
        INSERT INTO hello_world.World VALUES (i, MinVal + CEIL(RAND() * (MaxVal - MinVal)));
        SET i = i + 1;
    END WHILE;
    COMMIT;
END

CALL populate_world(10000, 0, 9999);

SELECT * FROM hello_world.World;

-- ----------------------------
-- Fortunes Table
-- ----------------------------

CREATE TABLE hello_world.Fortune (
  id INT NOT NULL AUTO_INCREMENT,
  message VARCHAR(255) CHARACTER SET utf8 COLLATE utf8_general_ci NOT NULL,
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