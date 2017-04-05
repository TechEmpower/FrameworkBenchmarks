create table fortune (
  id                            integer auto_increment not null,
  message                       varchar(255),
  constraint pk_fortune primary key (id)
);

create table world (
  id                            integer auto_increment not null,
  randomnumber                  integer,
  constraint pk_world primary key (id)
);

