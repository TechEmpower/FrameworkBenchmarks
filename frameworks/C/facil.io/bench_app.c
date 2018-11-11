/*
This is a short implementation fo the TechEmpower Framework Benchmarks. See:
http://frameworkbenchmarks.readthedocs.io/en/latest/

At the moment it's incomplete and only answers the plaintext and json tests
using the full HTTP framework stack (without any DB support).
*/
#include "http.h"

#include "fio_cli.h"
#include "fio_hashmap.h"

/* *****************************************************************************
Internal Helpers
***************************************************************************** */

/* initialize CLI helper and manage it's default options */
static void cli_init(int argc, char const *argv[]);

/* cleanup any leftovers */
static void cleanup(void);

/* reusable objects */
static FIOBJ HTTP_HEADER_SERVER;
static FIOBJ HTTP_VALUE_SERVER;
static FIOBJ JSON_KEY;
static FIOBJ JSON_VALUE;

/* *****************************************************************************
Routing
***************************************************************************** */

/* adds a route to our simple router */
static void route_add(char *path, void (*handler)(http_s *));

/* routes a request to the correct handler */
static void route_perform(http_s *);

/* cleanup for our router */
static void route_clear(void);

/* *****************************************************************************
Request handlers
***************************************************************************** */

/* handles JSON requests */
static void on_request_json(http_s *h);

/* handles plain text requests (Hello World) */
static void on_request_plain_text(http_s *h);

/* *****************************************************************************
The main function
***************************************************************************** */
int main(int argc, char const *argv[]) {
  /* initialize the CLI helper and options */
  cli_init(argc, argv);

  /* sertup routes */
  route_add("/json", on_request_json);
  route_add("/plaintext", on_request_plain_text);

  /* Server name and header */
  HTTP_HEADER_SERVER = fiobj_str_new("server", 6);
  HTTP_VALUE_SERVER = fiobj_strprintf("facil.io %u.%u.%u", FACIL_VERSION_MAJOR,
                                      FACIL_VERSION_MINOR, FACIL_VERSION_PATCH);
  /* JSON values to be serialized */
  JSON_KEY = fiobj_str_new("message", 7);
  JSON_VALUE = fiobj_str_new("Hello, World!", 13);

  /* Test for static file service */
  const char *public_folder = fio_cli_get_str("www");
  if (public_folder) {
    fprintf(stderr, "* serving static files from:%s\n", public_folder);
  }

  /* listen to HTTP connections */
  http_listen(fio_cli_get_str("port"), fio_cli_get_str("address"),
              .on_request = route_perform, .public_folder = public_folder,
              .log = fio_cli_get_int("log"));

  /* Start the facil.io reactor */
  facil_run(.threads = fio_cli_get_int("t"), .processes = fio_cli_get_int("w"));

  /* perform cleanup */
  cleanup();
  return 0;
}

/* *****************************************************************************
Request handlers
***************************************************************************** */

/* handles JSON requests */
static void on_request_json(http_s *h) {
  http_set_header(h, HTTP_HEADER_CONTENT_TYPE, http_mimetype_find("json", 4));
  FIOBJ json;
  /* create a new Hash to be serialized for every request */
  FIOBJ hash = fiobj_hash_new2(1);
  fiobj_hash_set(hash, JSON_KEY, fiobj_dup(JSON_VALUE));
  json = fiobj_obj2json(hash, 0);
  fiobj_free(hash);
  fio_cstr_s tmp = fiobj_obj2cstr(json);
  http_send_body(h, tmp.data, tmp.len);
  fiobj_free(json);
}

/* handles plain text requests (Hello World) */
static void on_request_plain_text(http_s *h) {
  http_set_header(h, HTTP_HEADER_CONTENT_TYPE, http_mimetype_find("txt", 3));
  http_send_body(h, "Hello, World!", 13);
}

/* *****************************************************************************
CLI
***************************************************************************** */

/* initialize CLI helper and manage it's default options */
static void cli_init(int argc, char const *argv[]) {
  fio_cli_start(argc, argv,
                "This is a facil.io framework benchmark application.\n"
                "\nFor details about the benchmarks visit:\n"
                "http://frameworkbenchmarks.readthedocs.io/en/latest/\n"
                "\nThe following arguments are supported:");
  fio_cli_accept_num("threads t",
                     "The number of threads to use. System dependent default.");
  fio_cli_accept_num(
      "workers w", "The number of processes to use. System dependent default.");
  fio_cli_accept_num(
      "port p", "The port number to listen to (set to 0 for Unix Sockets.");
  fio_cli_accept_str("address b", "The address to bind to.");
  fio_cli_accept_str("public www",
                     "A public folder for serve an HTTP static file service.");
  fio_cli_accept_bool("log v", "Turns logging on (logs to terminal).");
  fio_cli_accept_str("database db", "The database adrress.");
  fio_cli_accept_num("database-port dbp", "The database port.");

  /* setup default port */
  if (!fio_cli_get_str("p"))
    fio_cli_set_str("p", "8080");

  /* setup database address */
  if (!fio_cli_get_str("db")) {
    char *database = getenv("DBHOST");
    if (!database)
      database = "localhost";
    fio_cli_set_str("db", database);
  }
  /* setup database port - default for Redis */
  if (!fio_cli_get_str("dbp"))
    fio_cli_set_str("dbp", "6379");
}

/* *****************************************************************************
Routing
***************************************************************************** */

/* the router is a simple hash map */
static fio_hash_s routes;

/* adds a route to our simple router */
static void route_add(char *path, void (*handler)(http_s *)) {
  /* add handler to the hash map */
  size_t len = strlen(path);
  uint64_t hash = fio_siphash(path, len);
  fio_hash_insert(&routes, hash, (void *)(uintptr_t)handler);
}

/* routes a request to the correct handler */
static void route_perform(http_s *h) {
  /* add required Serevr header */
  http_set_header(h, HTTP_HEADER_SERVER, fiobj_dup(HTTP_VALUE_SERVER));
  /* collect path from hash map */
  fio_cstr_s tmp = fiobj_obj2cstr(h->path);
  uint64_t hash = fio_siphash(tmp.data, tmp.len);
  void (*handler)(http_s *) =
      (void (*)(http_s *))(uintptr_t)fio_hash_find(&routes, hash);
  /* forward request or send error */
  if (handler) {
    handler(h);
    return;
  }
  http_send_error(h, 404);
}

/* cleanup for our router */
static void route_clear(void) { fio_hash_free(&routes); }

/* *****************************************************************************
Cleanup
***************************************************************************** */

/* cleanup any leftovers */
static void cleanup(void) {
  fio_cli_end();
  fiobj_free(HTTP_HEADER_SERVER);
  fiobj_free(HTTP_VALUE_SERVER);
  fiobj_free(JSON_KEY);
  fiobj_free(JSON_VALUE);

  route_clear();
}
