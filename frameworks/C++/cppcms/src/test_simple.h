#ifndef TEST01_JSON_H
#define TEST01_JSON_H
#include <cppcms/application.h>

class test_simple : public cppcms::application {
public:
    test_simple(cppcms::service &s);
    void servre_json();
    void servre_plaintext();
private:

};

#endif /* TEST01_JSON_H */

