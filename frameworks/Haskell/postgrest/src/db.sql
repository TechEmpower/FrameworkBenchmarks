
create or replace function db() returns json as $$
   SELECT json_build_object('id', id, 'randomNumber', randomNumber) from (SELECT ((random()*9999)::int+1) as rnd) g JOIN "World" ON id = rnd; 
$$ language sql volatile;