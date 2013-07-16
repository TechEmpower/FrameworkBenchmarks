﻿-- This SQL Server T-SQL script creates and populates the World and Fortune tables.
--
-- To run this script, make sure that you've already run create-sqlserver-login-and-database.sql
-- to create the database user and database, then open a command prompt and run:
--
-- "%ProgramFiles%\Microsoft SQL Server\110\Tools\binn\sqlcmd.exe" -U benchmarkdbuser -P B3nchmarkDBPass -d hello_world -i <filename of this file>

IF OBJECT_ID('World', 'U') IS NOT NULL
    DROP TABLE World
GO

CREATE TABLE World (
  id int NOT NULL IDENTITY PRIMARY KEY,
  randomNumber int NOT NULL default 0
)
GO

-- Populate World table
DECLARE @RowCount INT
DECLARE @Random INT
SET @RowCount = 0

WHILE @RowCount < 10000
BEGIN
	SELECT @Random = ((10000 + 1) - 1) * RAND() + 1
	INSERT INTO World (randomNumber) VALUES (@Random)
	SET @RowCount = @RowCount + 1
END

GO

IF OBJECT_ID('Fortune', 'U') IS NOT NULL
    DROP TABLE Fortune
GO

-- Note that this uses nvarchar to make sure that the column is Unicode.
CREATE TABLE Fortune (
  id int NOT NULL IDENTITY PRIMARY KEY,
  message nvarchar(2048) NOT NULL
)
GO

INSERT INTO Fortune (message) VALUES (N'fortune: No such file or directory');
INSERT INTO Fortune (message) VALUES (N'A computer scientist is someone who fixes things that aren''t broken.');
INSERT INTO Fortune (message) VALUES (N'After enough decimal places, nobody gives a damn.');
INSERT INTO Fortune (message) VALUES (N'A bad random number generator: 1, 1, 1, 1, 1, 4.33e+67, 1, 1, 1');
INSERT INTO Fortune (message) VALUES (N'A computer program does what you tell it to do, not what you want it to do.');
INSERT INTO Fortune (message) VALUES (N'Emacs is a nice operating system, but I prefer UNIX. — Tom Christaensen');
INSERT INTO Fortune (message) VALUES (N'Any program that runs right is obsolete.');
INSERT INTO Fortune (message) VALUES (N'A list is only as strong as its weakest link. — Donald Knuth');
INSERT INTO Fortune (message) VALUES (N'Feature: A bug with seniority.');
INSERT INTO Fortune (message) VALUES (N'Computers make very fast, very accurate mistakes.');
INSERT INTO Fortune (message) VALUES (N'<script>alert("This should not be displayed in a browser alert box.");</script>');
INSERT INTO Fortune (message) VALUES (N'フレームワークのベンチマーク');
GO
