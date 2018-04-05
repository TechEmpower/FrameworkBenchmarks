# Servlet 3.1 API benchmarking test

This is Framework permutation based on the following technology stack

* Java
* Tomcat 9
* Servlet 3.1 with Async I/O
* Jackson 2 for JSON processing

Currently implemented test types are Plaintext and JSON. Their implementation comes in two flavors: PO (plain old) Servlets and Servlets 3.1 with Async I/O.

### Plaintext

* [Async](src/main/java/com/gitlab/zloster/tfb/servlet3/async/Plaintext.java)
* [Sync](src/main/java/com/gitlab/zloster/tfb/servlet3/sync/Plaintext.java)

### JSON

* [Async](src/main/java/com/gitlab/zloster/tfb/servlet3/async/JSON.java)
* [Sync](src/main/java/com/gitlab/zloster/tfb/servlet3/sync/JSON.java)

## Test URLs

### Default Maven profile

The `async` profile is activated by default.

* Plaintext - `http://localhost:8080/plaintext`
* JSON - `http://localhost:8080/json`

### `sync` Maven profile

* Plaintext - `http://localhost:8080/plaintext`
* JSON - `http://localhost:8080/json`
