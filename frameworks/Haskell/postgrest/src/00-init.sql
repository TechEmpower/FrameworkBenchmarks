create or replace function postgrest_init() returns void as $$
BEGIN
  CREATE TABLE IF NOT EXISTS "World" (id serial primary key not null, randomNumber int not null);
  IF NOT EXISTS (SELECT * FROM "World") THEN 
    INSERT INTO "World" SELECT nextval('"World_id_seq"'::regclass), (((random()*9999)::int+1))::int as randomNumber from generate_series(1, 10000) as id;
  END IF;

  CREATE TABLE IF NOT EXISTS "Fortunes" (id serial primary key not null, message text);
  IF NOT EXISTS (SELECT * FROM "Fortunes") THEN 
    INSERT INTO "Fortunes" (message) VALUES ('A'), ('B'), ('C'), ('D'), ('E'), ('F'), ('G'),('H'),('I'),('J'),('K'),('<script>alert(''foo'');</script>');
  END IF;
END
$$ language plpgsql volatile;

SELECT postgrest_init();


