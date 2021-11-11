#include "test_simple.h"

#include <cppcms/http_response.h>
#include <cppcms/url_dispatcher.h>
#include <cppcms/json.h>

#include  <ctime>

test_simple::test_simple(cppcms::service &s) : cppcms::application(s) {
    dispatcher().assign("/json", &test_simple::servre_json, this);
    dispatcher().assign("/plaintext", &test_simple::servre_plaintext, this);
}

void test_simple::servre_json() {
    response().content_type("application/json");
    cppcms::json::value my_object;
    my_object["message"] = "Hello, World!";
    response().date(std::time(nullptr));
    response().out() << my_object;
}

void test_simple::servre_plaintext() {
    response().content_type("text/plain");
    response().date(std::time(nullptr));
    response().out() << "Hello, World!";
}
