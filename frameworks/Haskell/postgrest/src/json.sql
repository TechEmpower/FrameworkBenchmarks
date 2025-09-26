create function jsonser() returns json as $$
   SELECT json_build_object('message', 'Hello, World!');
$$ language sql immutable;
