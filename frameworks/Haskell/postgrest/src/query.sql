create or replace function queries(queries text default '') returns jsonb as $$
DECLARE
   r "World"%ROWTYPE;
   j jsonb := jsonb_build_array();
   count int;
BEGIN
   IF queries ~ '^[1-9]\d{0,2}$' THEN
      count := CAST(queries as int);
   ELSE 
      count := 1;   
   END IF;
   IF count > 500 THEN
      count := 500;
   END IF;
   LOOP
      IF count <= 0 THEN
         EXIT;  -- exit loop
      END IF;
      SELECT id, randomNumber into r from (SELECT ((random()*9999)::int+1) as rnd) g JOIN "World" ON id = rnd; 
      j := jsonb_insert(j, '{0}', to_jsonb(r), true);
      count := count - 1;
   END LOOP;
   RETURN j;
END
$$ language plpgsql volatile;
