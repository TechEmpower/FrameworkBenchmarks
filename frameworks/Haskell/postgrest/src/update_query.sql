create or replace function update(queries text default '') returns jsonb as $$
DECLARE
   r "World"%ROWTYPE;
   j jsonb := jsonb_build_array();
   new_rnd int;
   rnd_id int;
   count int;
BEGIN
   IF queries ~  '^\d+$' THEN
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
      rnd_id := ((random()*9999)::int+1);
      new_rnd := ((random()*9999)::int+1);
      UPDATE "World" SET randomNumber = new_rnd WHERE id = rnd_id RETURNING * INTO r;
      j := jsonb_insert(j, '{0}', to_jsonb(r), true);
      count := count - 1;
   END LOOP;
   RETURN j;
END
$$ language plpgsql volatile;
