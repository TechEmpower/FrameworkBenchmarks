CREATE TABLE uw_Bench_world AS SELECT uw_id::int8, trunc(random()*9999+1)::int8 AS uw_randomnumber FROM generate_series(1,10000) AS uw_id;
ALTER TABLE uw_Bench_world ADD PRIMARY KEY (uw_id);
ALTER TABLE uw_Bench_world ALTER COLUMN uw_randomnumber SET NOT NULL;
