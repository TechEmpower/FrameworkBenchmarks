create or replace function custom_headers() returns void as $$
declare
  user_agent text := current_setting('request.header.user-agent', true);
begin
  if user_agent similar to '%MSIE (6.0|7.0)%' then
    perform set_config('response.headers',
      '[{"Cache-Control": "no-cache, no-store, must-revalidate"}]', false);
  end if;
end; $$ language plpgsql;