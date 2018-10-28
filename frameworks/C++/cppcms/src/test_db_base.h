#ifndef TEST_DB_BASE_H
#define TEST_DB_BASE_H

#include <cppcms/application.h>
#include <cppdb/frontend.h>

#include <string>

class test_db_base : public cppcms::application {
public:
    test_db_base(cppcms::service &s);
    virtual void init();
    virtual void clear();
protected:
    cppdb::session sql;
private:
    std::string db_connection_str;
};

#endif /* TEST_DB_BASE_H */

