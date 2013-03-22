CREATE SEQUENCE authors_seq;
CREATE TABLE authors(
	author_id INT NOT NULL PRIMARY KEY,
	parent_author_id INT,
	name VARCHAR(25) DEFAULT 'default_name' NOT NULL,
	updated_at timestamp,
	created_at timestamp,
	some_date date,
	--some_time time,
	some_text varchar2(100),
	encrypted_password varchar(50),
	"mixedCaseField" varchar(50)
);

CREATE SEQUENCE books_seq;
CREATE TABLE books(
	book_id INT NOT NULL PRIMARY KEY,
	Author_Id INT,
	secondary_author_id INT,
	name VARCHAR(50),
	numeric_test VARCHAR(10) DEFAULT '0',
	special NUMERIC(10,2) DEFAULT 0);

CREATE SEQUENCE venues_seq;
CREATE TABLE venues (
  Id INT NOT NULL PRIMARY KEY,
  name varchar(50),
  city varchar(60),
  state char(2),
  address varchar(50),
  phone varchar(10) default NULL,
  UNIQUE(name,address)
);

CREATE SEQUENCE events_seq;
CREATE TABLE events (
  id INT NOT NULL PRIMARY KEY,
  venue_id int NOT NULL,
  host_id int NOT NULL,
  title varchar(60) NOT NULL,
  description varchar(10),
  type varchar(15) default NULL
);

CREATE SEQUENCE hosts_seq;
CREATE TABLE hosts(
	id INT NOT NULL PRIMARY KEY,
	name VARCHAR(25)
);

CREATE SEQUENCE employees_seq;
CREATE TABLE employees (
	id INT NOT NULL PRIMARY KEY,
	first_name VARCHAR( 255 ) NOT NULL ,
	last_name VARCHAR( 255 ) NOT NULL ,
	nick_name VARCHAR( 255 ) NOT NULL
);

CREATE SEQUENCE positions_seq;
CREATE TABLE positions (
  id INT NOT NULL PRIMARY KEY,
  employee_id int NOT NULL,
  title VARCHAR(255) NOT NULL,
  active SMALLINT NOT NULL
);

CREATE SEQUENCE awesome_people_seq;
CREATE TABLE awesome_people(
	id int not null primary key,
	author_id int,
	is_awesome int default 1
);

CREATE SEQUENCE amenities_seq;
CREATE TABLE amenities(
  amenity_id int primary key,
  type varchar(40) NOT NULL
);

CREATE SEQUENCE property_seq;
CREATE TABLE property(
  property_id int primary key
);

CREATE SEQUENCE property_amenities_seq;
CREATE TABLE property_amenities(
  id int primary key,
  amenity_id int not null,
  property_id int not null
);

CREATE SEQUENCE valuestore_seq;
CREATE TABLE valuestore(
  id int primary key,
 `key` varchar(20) NOT NULL DEFAULT '',
 `value` varchar(255) NOT NULL DEFAULT ''
);