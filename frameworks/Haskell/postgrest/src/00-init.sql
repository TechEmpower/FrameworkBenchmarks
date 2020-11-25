DROP TABLE IF EXISTS "World";
CREATE TABLE "World" (id serial primary key not null, randomNumber int not null);
INSERT INTO "World" SELECT nextval('"World_id_seq"'::regclass), (((random()*9999)::int+1))::int as randomNumber from generate_series(1, 10000) as id;

DROP TABLE IF EXISTS "Fortunes";

CREATE TABLE "Fortunes" (id serial primary key not null, message text);
INSERT INTO "Fortunes" (message) VALUES ('A'), ('B'), ('C'), ('D'), ('E'), ('F'), ('G'),('H'),('I'),('J'),('K'),('<script>alert(''foo'');</script>');
