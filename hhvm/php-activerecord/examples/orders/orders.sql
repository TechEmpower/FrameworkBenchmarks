-- written for mysql, not tested with any other db

drop table if exists people;
create table people(
  id int not null primary key auto_increment,
  name varchar(50),
  state char(2),
  created_at datetime,
  updated_at datetime
);

drop table if exists orders;
create table orders(
  id int not null primary key auto_increment,
  person_id int not null,
  item_name varchar(50),
  price decimal(10,2),
  tax decimal(10,2),
  created_at datetime
);

drop table if exists payments;
create table payments(
  id int not null primary key auto_increment,
  order_id int not null,
  person_id int not null,
  amount decimal(10,2),
  created_at datetime
);
