create table books(
  id int not null primary key auto_increment,
  name varchar(50),
  author varchar(50)
);

insert into books(name,author) values('How to be Angry','Jax');
