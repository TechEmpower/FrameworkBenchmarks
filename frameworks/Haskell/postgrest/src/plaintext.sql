create domain "text/plain" text;

create function plaintext() returns "text/plain" as $$
   SELECT 'Hello, World!';
$$ language sql immutable;
