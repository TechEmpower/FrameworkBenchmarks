-- This SQL Server T-SQL script creates the database user and hello_world database.
--
-- To run this script, login to an administrator account in Windows, open a command prompt and run:
--
-- "%ProgramFiles%\Microsoft SQL Server\110\Tools\binn\sqlcmd.exe" -i <filename of this file>
--

-- This password has mixed-case and a number to satisfy the Windows password policy
CREATE LOGIN benchmarkdbuser WITH PASSWORD = 'B3nchmarkDBPass'
GO

IF EXISTS(SELECT * FROM SYS.DATABASES WHERE NAME='hello_world')
DROP DATABASE hello_world
GO

CREATE DATABASE hello_world
GO
USE hello_world
GO

-- Give this user total power over the database
CREATE USER benchmarkdbuser FOR LOGIN benchmarkdbuser
EXEC sp_addrolemember 'db_owner', 'benchmarkdbuser'
GO
