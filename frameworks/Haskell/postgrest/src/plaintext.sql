create function plaintext() returns text as $$
   SELECT 'Hello, World!';
$$ language sql volatile;