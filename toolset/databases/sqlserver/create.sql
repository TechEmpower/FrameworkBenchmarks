create database hello_world;
go
use hello_world;
go
create table world (
  id int not null identity(1,1) primary key,
  randomnumber int not null
);
go
set nocount on;
go
declare @i int = 0;
while @i < 10000
begin
  insert into world (randomnumber) values (1 + cast(rand(checksum(newid())) * 10000 as int));
  set @i = @i + 1;
end;
go
create table fortune (
  id int not null identity(1,1) primary key,
  message nvarchar(2048) not null
);
go
insert into fortune (message) values
(N'fortune: No such file or directory'),
(N'A computer scientist is someone who fixes things that aren''t broken.'),
(N'After enough decimal places, nobody gives a damn.'),
(N'A bad random number generator: 1, 1, 1, 1, 1, 4.33e+67, 1, 1, 1'),
(N'A computer program does what you tell it to do, not what you want it to do.'),
(N'Emacs is a nice operating system, but I prefer UNIX. — Tom Christaensen'),
(N'Any program that runs right is obsolete.'),
(N'A list is only as strong as its weakest link. — Donald Knuth'),
(N'Feature: A bug with seniority.'),
(N'Computers make very fast, very accurate mistakes.'),
(N'<script>alert("This should not be displayed in a browser alert box.");</script>'),
(N'フレームワークのベンチマーク');
go
set nocount off;
go
exec sp_configure 'contained database authentication', 1
go
reconfigure
go
alter database hello_world set containment = partial;
go
create login benchmarkdbuser with password = 'benchmarkdbpass', check_policy = off;
go
create user benchmarkdbuser for login benchmarkdbuser;
go
grant select, update on world to benchmarkdbuser;
grant select on fortune to benchmarkdbuser;
go
