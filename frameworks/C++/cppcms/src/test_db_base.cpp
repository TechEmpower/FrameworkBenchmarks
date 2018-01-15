#include "test_db_base.h"

#include <cppcms/json.h>

test_db_base::test_db_base(cppcms::service &srv) : cppcms::application(srv) {
    db_connection_str = settings().get<std::string>("app.db_connection_string");
}

void test_db_base::init() {
    sql.open(db_connection_str);
}

void test_db_base::clear() {
    sql.close();
}