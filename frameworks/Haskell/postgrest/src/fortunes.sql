create domain "text/html" as text;

create or replace function sanitize_html(text) returns text as $$
  select replace(replace(replace(replace(replace($1, '&', '&amp;'), '"', '&quot;'),'>', '&gt;'),'<', '&lt;'), '''', '&apos;')
$$ language sql immutable;

create or replace function fortune_template("Fortune") returns text as $$
   SELECT format('<tr><td>%s</td><td>%s</td></tr>', $1.id, sanitize_html($1.message));
$$ language sql immutable;

create or replace function fortunes() returns "text/html" as $$
   select set_config('response.headers', '[{"Content-Type": "text/html; charset=utf-8"}]', true);

   select '<!DOCTYPE html><html><head><title>Fortunes</title></head><body><table><tr><th>id</th><th>message</th></tr>'
      || string_agg(fortune_template(f), NULL order by f.message collate unicode asc)
      || '</table></body></html>'
   from (select * from "Fortune" union all select 0, 'Additional fortune added at request time.') f;
$$ language sql volatile;
