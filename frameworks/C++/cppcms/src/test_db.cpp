#include "test_db.h"
#include <cppcms/http_response.h>
#include <cppcms/url_dispatcher.h>
#include <cppcms/json.h>
#include <cppcms/cache_interface.h>
#include <cppcms/serialization.h>

#include  <ctime>

class world_object : public cppcms::serializable {
public:
    int id;
    int randomNumber;

    void serialize(cppcms::archive &a) {
        a & id & randomNumber;
    }
};

test_db::test_db(cppcms::service &s) : test_db_base(s) {
    srand(std::time(nullptr));
    dispatcher().assign("/db", &test_db::servre_db, this);
    dispatcher().assign("/queries/(.{0,3}).*", &test_db::servre_queries, this, 1);
    dispatcher().assign("/updates/(.{0,3}).*", &test_db::servre_updates, this, 1);
    dispatcher().assign("/cached-worlds/(.{0,3}).*", &test_db::servre_cached_worlds, this, 1);

}

void test_db::servre_db() {
    int r = get_random_id();
    world_object wo;
    if (get_world_object_db(wo, r)) {
        cppcms::json::value my_object;
        my_object["id"] = wo.id;
        my_object["randomNumber"] = wo.randomNumber;
        send_json(my_object);
    }
}

void test_db::servre_queries(const std::string val) {
    int count = cinvert_constraint_count(val);
    cppcms::json::array my_object;

    for (int i = 0; i < count; ++i) {
        cppcms::json::value my_val;
        int r = get_random_id();

        world_object wo;
        if (get_world_object_db(wo, r)) {
            my_val["id"] = wo.id;
            my_val["randomNumber"] = wo.randomNumber;
            my_object.push_back(my_val);
        } else {
            return;
        }
    }

    send_json(my_object);
}

void test_db::servre_updates(const std::string val) {
    int count = cinvert_constraint_count(val);
    cppcms::json::array my_object;

    for (int i = 0; i < count; ++i) {
        cppcms::json::value my_val;
        int r = get_random_id();

        world_object wo;
        if (get_world_object_db(wo, r)) {
            my_val["id"] = wo.id;
            my_val["randomNumber"] = wo.randomNumber;
            int rv = get_random_value();
            wo.randomNumber = rv;
            if (!update_world_object_db(wo)) {
                return;
            }

            my_object.push_back(my_val);
        } else {
            return;
        }
    }

    send_json(my_object);
}

void test_db::servre_cached_worlds(const std::string val) {
    int count = cinvert_constraint_count(val);
    cppcms::json::array my_object;
    for (int i = 0; i < count; ++i) {
        cppcms::json::value my_val;
        int r = get_random_id();
        world_object wo;
        if (!cache().fetch_data(std::to_string(r), wo)) {
            if (get_world_object_db_ch(wo, r)) {
                cache().store_data(std::to_string(r), wo);
            } else {
                return;
            }
        }
        my_val["id"] = wo.id;
        my_val["randomNumber"] = wo.randomNumber;
        my_object.push_back(my_val);
    }

    send_json(my_object);


}

int test_db::get_random_id() {
    return rand() % (MAX_WORLD_ID - 1) + 1;
}

int test_db::get_random_value() {
    return rand() % (MAX_RANDOM_VALUE - 1) + 1;
}

int test_db::cinvert_constraint_count(const std::string & val) {
    int count = 0;
    count = atoi(val.c_str());
    if (count < 1)count = 1;
    if (count > MAX_QUERY_COUNT)count = MAX_QUERY_COUNT;
    return count;
}

//For CachedWorld

bool test_db::get_world_object_db_ch(world_object &c, int id) {
    cppdb::result res;
    res = sql << "SELECT id, randomNumber FROM world WHERE id = ?" << id << cppdb::row;
    if (res.empty()) {
        response().make_error_response(response().internal_server_error, "SQL result empty");
        return false;
    }
    res >> c.id >> c.randomNumber;
    return true;
}

bool test_db::get_world_object_db(world_object &c, int id) {
    cppdb::result res;
    res = sql << "SELECT id, randomNumber FROM world WHERE id = ?" << id << cppdb::row;
    if (res.empty()) {
        response().make_error_response(response().internal_server_error, "SQL result empty");
        return false;
    }
    res >> c.id >> c.randomNumber;
    return true;
}

bool test_db::update_world_object_db(const world_object &c) {
    cppdb::statement st = sql
            << "UPDATE world SET randomNumber = ? WHERE id = ?"
            << c.randomNumber << c.id;
    st.exec();
    return true;
}

void test_db::send_json(const cppcms::json::value & json) {
    response().content_type("application/json");
    response().date(std::time(nullptr));
    response().out() << json;
}


