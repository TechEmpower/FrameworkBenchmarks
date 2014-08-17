CREATE TABLE IF NOT EXISTS companies (
  id SERIAL NOT NULL,
  name varchar(255),
  active boolean,
  created TIMESTAMP(0) WITH TIME ZONE,
  modified TIMESTAMP(0) WITH TIME ZONE
);
