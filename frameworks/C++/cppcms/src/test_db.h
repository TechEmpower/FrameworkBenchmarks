#ifndef TEST02_DB_H
#define TEST02_DB_H
#include "test_db_base.h"

class world_object;

class test_db : public test_db_base {
    const int MAX_WORLD_ID = 10000;
    const int MAX_RANDOM_VALUE = 10000;
    const int MAX_QUERY_COUNT = 500;
public:
    test_db(cppcms::service &s);
    void servre_db();
    void servre_queries(const std::string a);
    void servre_updates(const std::string a);
    void servre_cached_worlds(const std::string a);
private:
    bool update_world_object_db(const world_object &c);
    bool get_world_object_db(world_object &c, int id);
    bool get_world_object_db_ch(world_object &c, int id);
    int cinvert_constraint_count(const std::string & val);
    int get_random_id();
    int get_random_value();
    void send_json(const cppcms::json::value & json);
};



#endif /* TEST02_DB_H */

