create table simple_book(
  book_id int not null primary key auto_increment,
  name varchar(50)
);

insert into simple_book (name) values ('simple w/ options!');
