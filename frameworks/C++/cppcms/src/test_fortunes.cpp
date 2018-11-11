#include "test_fortunes.h"

#include "data.h"

#include <cppcms/http_response.h>
#include <cppcms/url_dispatcher.h>
#include <cppcms/json.h>

#include <algorithm>
#include <ctime>

test_fortunes::test_fortunes(cppcms::service &s) : test_db_base(s) {
    dispatcher().assign("", &test_fortunes::servre, this);
}

void test_fortunes::servre() {
    data_holder c;
    get_fortunes_db(c);

    fortune fortune_object;
    fortune_object.id = 0;
    fortune_object.message = "Additional fortune added at request time.";
    c.fortunes.push_back(fortune_object);

    std::sort(c.fortunes.begin(), c.fortunes.end(), [ ](const fortune& a, const fortune & b) {
        return a.message.compare(b.message) < 0;
    });

    response().date(std::time(nullptr));
    render("message", c);
}

bool test_fortunes::get_fortunes_db(data_holder &dh) {
    fortune_vector &c = dh.fortunes;
    cppdb::result res;
    res = sql << "SELECT id, message FROM fortune";
    while (res.next()) {
        fortune f;
        res >> f.id >> f.message;
        c.push_back(f);
    }
    return true;
}
