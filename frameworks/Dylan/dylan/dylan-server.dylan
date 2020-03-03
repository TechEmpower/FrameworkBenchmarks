Module: dylan-server

define class <hello-page> (<resource>)
end;

define method respond (page :: <hello-page>, #key)
  let stream = current-response();
  set-header(stream, "Content-Type", "text/plain;charset=utf-8");
  set-header(stream, "Date", as-rfc1123-string(current-date()));
  write(stream, "Hello, World!");
end;

let server = make(<http-server>, listeners: list("0.0.0.0:8080"));

add-resource(server, "/plaintext", make(<hello-page>));

start-server(server);
