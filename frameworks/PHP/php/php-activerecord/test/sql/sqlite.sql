CREATE TABLE authors(
	author_id INTEGER NOT NULL PRIMARY KEY,
	parent_author_id INT,
	name VARCHAR  (25) NOT NULL DEFAULT default_name, -- don't touch those spaces
	updated_at datetime,
	created_at datetime,
	some_Date date,
	some_time time,
	some_text text,
	encrypted_password varchar(50),
	mixedCaseField varchar(50)
);

CREATE TABLE books(
	book_id INTEGER NOT NULL PRIMARY KEY,
	Author_Id INT,
	secondary_author_id INT,
	name VARCHAR(50),
	numeric_test VARCHAR(10) DEFAULT '0',
	special NUMERIC(10,2) DEFAULT 0
);

CREATE TABLE venues (
  Id INTEGER NOT NULL PRIMARY KEY,
  name varchar(50),
  city varchar(60),
  state char(2),
  address varchar(50),
  phone varchar(10) default NULL,
  UNIQUE(name,address)
);

CREATE TABLE events (
  id INTEGER NOT NULL PRIMARY KEY,
  venue_id int NOT NULL,
  host_id int NOT NULL,
  title varchar(60) NOT NULL,
  description varchar(10),
  type varchar(15) default NULL
);

CREATE TABLE hosts(
	id INT NOT NULL PRIMARY KEY,
	name VARCHAR(25)
);

CREATE TABLE employees (
	id INTEGER NOT NULL PRIMARY KEY,
	first_name VARCHAR( 255 ) NOT NULL ,
	last_name VARCHAR( 255 ) NOT NULL ,
	nick_name VARCHAR( 255 ) NOT NULL
);

CREATE TABLE positions (
  id INTEGER NOT NULL PRIMARY KEY,
  employee_id int NOT NULL,
  title VARCHAR(255) NOT NULL,
  active SMALLINT NOT NULL
);

CREATE TABLE `rm-bldg`(
    `rm-id` INT NOT NULL,
    `rm-name` VARCHAR(10) NOT NULL,
    `space out` VARCHAR(1) NOT NULL
);

CREATE TABLE awesome_people(
	id integer not null primary key,
	author_id int,
	is_awesome int default 1
);

CREATE TABLE amenities(
  `amenity_id` INTEGER NOT NULL PRIMARY KEY,
  `type` varchar(40) DEFAULT NULL
);

CREATE TABLE property(
  `property_id` INTEGER NOT NULL PRIMARY KEY
);

CREATE TABLE property_amenities(
  `id` INTEGER NOT NULL PRIMARY KEY,
  `amenity_id` INT NOT NULL,
  `property_id` INT NOT NULL
);

CREATE TABLE users (
    id INTEGER NOT NULL PRIMARY KEY
);

CREATE TABLE newsletters (
    id INTEGER NOT NULL PRIMARY KEY
);

CREATE TABLE user_newsletters (
    id INTEGER NOT NULL PRIMARY KEY,
    user_id INTEGER NOT NULL,
    newsletter_id INTEGER NOT NULL
);

CREATE TABLE valuestore (
  `id` INTEGER NOT NULL PRIMARY KEY,
  `key` varchar(20) NOT NULL DEFAULT '',
  `value` varchar(255) NOT NULL DEFAULT ''
);
