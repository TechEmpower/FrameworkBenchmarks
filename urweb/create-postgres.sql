DROP TABLE IF EXISTS uw_Bench_world;

CREATE TABLE uw_Bench_world AS
SELECT uw_id::int8, trunc(random()*9999+1)::int8 AS uw_randomnumber
FROM generate_series(1,10000) AS uw_id;

ALTER TABLE uw_Bench_world ADD PRIMARY KEY (uw_id);
ALTER TABLE uw_Bench_world ALTER COLUMN uw_randomnumber SET NOT NULL;

DROP TABLE IF EXISTS uw_Bench_fortune;
CREATE TABLE uw_Bench_fortune (
  uw_id int8 NOT NULL,
  uw_message text NOT NULL,
  PRIMARY KEY (uw_id)
);

INSERT INTO uw_Bench_fortune (uw_id, uw_message) VALUES (11, '<script>alert("This should not be displayed in a browser alert box.")</script>');
INSERT INTO uw_Bench_fortune (uw_id, uw_message) VALUES (4, 'A bad random number generator: 1, 1, 1, 1, 1, 4.33e+67, 1, 1, 1');
INSERT INTO uw_Bench_fortune (uw_id, uw_message) VALUES (5, 'A computer program does what you tell it to do, not what you want it to do.');
INSERT INTO uw_Bench_fortune (uw_id, uw_message) VALUES (2, 'A computer scientist is someone who fixes things that aren''t broken.');
INSERT INTO uw_Bench_fortune (uw_id, uw_message) VALUES (8, 'A list is only as strong as its weakest link. — Donald Knuth');
INSERT INTO uw_Bench_fortune (uw_id, uw_message) VALUES (3, 'After enough decimal places, nobody gives a damn.');
INSERT INTO uw_Bench_fortune (uw_id, uw_message) VALUES (7, 'Any program that runs right is obsolete.');
INSERT INTO uw_Bench_fortune (uw_id, uw_message) VALUES (10, 'Computers make very fast, very accurate mistakes.');
INSERT INTO uw_Bench_fortune (uw_id, uw_message) VALUES (6, 'Emacs is a nice operating system, but I prefer UNIX. — Tom Christaensen');
INSERT INTO uw_Bench_fortune (uw_id, uw_message) VALUES (9, 'Feature: A bug with seniority.');
INSERT INTO uw_Bench_fortune (uw_id, uw_message) VALUES (1, 'fortune: No such file or directory');
INSERT INTO uw_Bench_fortune (uw_id, uw_message) VALUES (12, 'フレームワークのベンチマーク');
