/* -*- Mode: C; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 * Duda I/O Benchmark Tests
 * ========================
 * This web service is made for the performance contest made by
 * TechEmpower, mode details here:
 *
 *     http://www.techempower.com/benchmarks
 *
 * At the moment only Tests 1 & 6 are implemented.
 */

#include "webservice.h"
#include "packages/json/json.h"

/* Test Macros (Tn) */
#define JSON_CONTENT_TYPE    "Content-Type: application/json"
#define PLAIN_CONTENT_TYPE   "Content-Type: text/plain"
#define T6_BODY              "Hello, World!"

DUDA_REGISTER("Duda I/O Benchmark Test", "WS Bench");

/*
 * Test type 1: JSON serialization
 * ===============================
 * This test use the JSON API object to compose the JSON response
 */
void cb_json(duda_request_t *dr)
{
    char *body;
    int body_len;
    json_t *j_root;

    /* Instance the JSON object and compose the content */
    j_root = json->create_object();
    json->add_to_object(j_root,
                        "message",
                        json->create_string("Hello, World!"));

    /* Format output to string */
    body = json->print_unformatted_gc(dr, j_root);
    body_len = strlen(body);

    /* Delete the JSON tree */
    json->delete(j_root);

    /* Compose the response */
    response->http_status(dr, 200);
    response->http_header_n(dr, JSON_CONTENT_TYPE, sizeof(JSON_CONTENT_TYPE) - 1);
    response->print(dr, body, body_len);
    response->end(dr, NULL);
}


/*
 * Test type 6: Plaintext
 * ======================
 */
void cb_plaintext(duda_request_t *dr)
{
    response->http_status(dr, 200);
    response->http_header_n(dr, PLAIN_CONTENT_TYPE, sizeof(PLAIN_CONTENT_TYPE) - 1);
    response->print(dr, T6_BODY, sizeof(T6_BODY) - 1);
    response->end(dr, NULL);
}

int duda_main()
{
    /* load packages */
    duda_load_package(json, "json");

    /* let this web service own the virtual host */
    conf->service_root();

    /* set callbacks */
    map->static_add("/json", "cb_json");            /* Test #1 */
    map->static_add("/plaintext", "cb_plaintext");  /* Test #6 */

    return 0;
}
