#ifndef TEST04_FORTUNES_H
#define TEST04_FORTUNES_H
#include "test_db_base.h"


class data_holder;

class test_fortunes : public test_db_base {
public:
    test_fortunes(cppcms::service &s);
    bool get_fortunes_db(data_holder &c);
    void servre();
private:

};

#endif /* TEST04_FORTUNES_H */

