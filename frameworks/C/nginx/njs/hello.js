function hello(r) {
  r.headersOut["Content-Type"] = "text/plain; charset=UTF-8";
  r.return(200, "Hello, World!");
}

function json(r) {
  r.headersOut["Content-Type"] = "application/json";
  r.return(200, JSON.stringify({ message: "Hello, World!" }));
}

export default { hello, json };
