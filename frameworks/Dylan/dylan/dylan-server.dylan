Module: dylan-server

define class <plaintext> (<resource>)
end;

define method respond (plaintext :: <plaintext>, #key)
  let stream = current-response();
  set-header(stream, "Content-Type", "text/plain;charset=utf-8");
  set-header(stream, "Date", as-rfc1123-string(current-date()));
  write(stream, "Hello, World!");
end;

define class <json> (<resource>)
end;

define method respond (json :: <json>, #key)
  let stream = current-response();
  set-header(stream, "Content-Type", "application/json");
  set-header(stream, "Date", as-rfc1123-string(current-date()));
  print(table("message" => "Hello, World!"), stream);
end;

let server = make(<http-server>, listeners: list("0.0.0.0:8080"));

add-resource(server, "/plaintext", make(<plaintext>));
add-resource(server, "/json", make(<json>));

start-server(server);
