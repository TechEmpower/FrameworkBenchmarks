CREATE TABLE authors(
	author_id SERIAL PRIMARY KEY,
	parent_author_id INT,
	name VARCHAR(25) NOT NULL DEFAULT 'default_name',
	updated_at timestamp,
	created_at timestamp,
	"some_Date" date,
	some_time time,
	some_text text,
	encrypted_password varchar(50),
	"mixedCaseField" varchar(50)
);

CREATE TABLE books(
	book_id SERIAL PRIMARY KEY,
	author_id INT,
	secondary_author_id INT,
	name VARCHAR(50),
	numeric_test VARCHAR(10) DEFAULT '0',
	special NUMERIC(10,2) DEFAULT 0.0
);

CREATE TABLE venues (
	id SERIAL PRIMARY KEY,
	name varchar(50),
	city varchar(60),
	state char(2),
	address varchar(50),
	phone varchar(10) default NULL,
	UNIQUE(name,address)
);

CREATE TABLE events (
	id SERIAL PRIMARY KEY,
	venue_id int NOT NULL,
	host_id int NOT NULL,
	title varchar(60) NOT NULL,
	description varchar(10),
	type varchar(15) default NULL
);

CREATE TABLE hosts(
	id SERIAL PRIMARY KEY,
	name VARCHAR(25)
);

CREATE TABLE employees (
	id SERIAL PRIMARY KEY,
	first_name VARCHAR(255) NOT NULL,
	last_name VARCHAR(255) NOT NULL,
	nick_name VARCHAR(255) NOT NULL
);

CREATE TABLE positions (
	id SERIAL PRIMARY KEY,
	employee_id int NOT NULL,
	title VARCHAR(255) NOT NULL,
	active SMALLINT NOT NULL
);

CREATE TABLE "rm-bldg"(
    "rm-id" SERIAL PRIMARY KEY,
    "rm-name" VARCHAR(10) NOT NULL,
    "space out" VARCHAR(1) NOT NULL
);

CREATE TABLE awesome_people(
	id serial primary key,
	author_id int,
	is_awesome int default 1
);

CREATE TABLE amenities(
	amenity_id serial primary key,
	type varchar(40) NOT NULL
);

CREATE TABLE property(
	property_id serial primary key
);

CREATE TABLE property_amenities(
	id serial primary key,
	amenity_id int not null,
	property_id int not null
);

CREATE TABLE users(
	id serial primary key
);

CREATE TABLE newsletters(
	id serial primary key
);

CREATE TABLE user_newsletters(
  id serial primary key,
  user_id int not null,
  newsletter_id int not null
);

CREATE TABLE valuestore (
  id serial primary key,
  key varchar(20) NOT NULL DEFAULT '',
  value varchar(255) NOT NULL DEFAULT ''
);

-- reproduces issue GH-96 for testing
CREATE INDEX user_newsletters_id_and_user_id_idx ON user_newsletters USING btree(id, user_id);
