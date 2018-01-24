#include <cppcms/service.h>
#include <cppcms/applications_pool.h>

#include <cppcms/http_response.h>
#include <cppcms/url_dispatcher.h>

#include <iostream>

#include "test_simple.h"
#include "test_fortunes.h"
#include "test_db.h"

class myapp : public cppcms::application {
public:

    myapp(cppcms::service &srv) : cppcms::application(srv) {

        attach(new test_simple(srv), "/json()", 0);
        attach(new test_simple(srv), "/plaintext()", 0);

        attach(new test_fortunes(srv), "/fortunes()", 1);

        attach(new test_db(srv), "/db()", 0);
        attach(new test_db(srv), "/queries/.*", 0);
        attach(new test_db(srv), "/updates/.*", 0);
        attach(new test_db(srv), "/cached-worlds/.*", 0);

    }

};

int main(int argc, char **argv) {
    try {
        cppcms::service app(argc, argv);
        app.applications_pool().mount(cppcms::applications_factory<myapp>());
        app.run();
    } catch (std::exception const &e) {
        std::cerr << e.what() << std::endl;
    }
}
