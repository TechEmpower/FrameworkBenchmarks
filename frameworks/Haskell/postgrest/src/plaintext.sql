create domain "text/plain" as text;

create function plaintext() returns "text/plain" as $$
   SELECT 'Hello, World!';
$$ language sql immutable;
