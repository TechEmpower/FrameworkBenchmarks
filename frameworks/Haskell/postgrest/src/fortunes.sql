CREATE TYPE fortune_t AS (id int, message text);

create or replace function fortune_template(f fortune_t) returns text as $$
   SELECT format('<tr><td>%s</td><td>%s</td></tr>', $1.id, regexp_replace($1.message, '<', '&lt;','g')); 
$$ language sql volatile;

create or replace function fortunes_template(fortunes fortune_t[]) returns text as $$
WITH header AS (
   SELECT 0 as id,'<!DOCTYPE html>
<html><head><title>Fortunes</title></head><body><table><tr><th>id</th><th>message</th></tr>' as html
), footer AS (
   SELECT 2,'</table></body></html>' as html
), fortunes AS (
   SELECT unnest as fortune from unnest($1) 
), additional AS (
   SELECT (-1, 'Additional fortune added at request time.')::fortune_t as f
), all_fortunes AS (
   SELECT * from (SELECT * FROM fortunes UNION ALL SELECT * from additional) p ORDER BY (fortune).message
), fortunes_html AS (
   SELECT 1,string_agg(fortune_template(fortune), '') from all_fortunes
), html AS (
   SELECT * FROM header UNION SELECT * FROM fortunes_html UNION SELECT * from footer ORDER BY id
)
SELECT string_agg(html,'') from html;
$$ language sql volatile;

create or replace function "fortunes.html"() returns bytea as $$
DECLARE
   fortunes fortune_t[];
BEGIN
   SET LOCAL "response.headers" = '[{"Content-Type": "text/html"}]';
   SELECT array_agg(CAST((id,message) AS fortune_t)) FROM "Fortunes" INTO fortunes;
   RETURN convert_to(fortunes_template(fortunes), 'UTF8');
END
$$ language plpgsql volatile;
