#include "orm.h"
#include <chrono>
#include <thread>
#include <algorithm>
#include <random>
#include <chrono>
#include "httppeer.h"
#include "techempower.h"
#include "datetime.h"
#include "func.h"
#include "pzcache.h"
#include "json_reflect_headers.h"
#include "techempower_json.h"
namespace http
{
    //@urlpath(null,plaintext)
    std::string techempowerplaintext(std::shared_ptr<httppeer> peer)
    {
        peer->type("text/plain; charset=UTF-8");
        peer->set_header("Date", get_gmttime());
        peer->output = "Hello, World!";
        return "";
    }

    //@urlpath(null,json)
    std::string techempowerjson(std::shared_ptr<httppeer> peer)
    {
        peer->type("application/json; charset=UTF-8");
        peer->set_header("Date", get_gmttime());
        struct techempower_outjson_t a;
        a.message = "Hello, World!";
        peer->output = json_encode(a);
        return "";
    }

    //@urlpath(null,db)
    std::string techempowerdb(std::shared_ptr<httppeer> peer)
    {
        peer->type("application/json; charset=UTF-8");
        peer->set_header("Date", get_gmttime());
        auto myworld = orm::World();
        unsigned int rd_num = rand_range(1, 10000);
        myworld.get_one(rd_num);

        peer->output = myworld.data_tojson();
        return "";
    }

    //@urlpath(null,queries)
    std::string techempowerqueries(std::shared_ptr<httppeer> peer)
    {
        peer->type("application/json; charset=UTF-8");
        peer->set_header("Date", get_gmttime());

        unsigned int get_num = peer->get["queries"].to_int();
        if (get_num == 0)
        {
            get_num = 1;
        }
        else if (get_num > 500)
        {
            get_num = 500;
        }
        auto myworld = orm::World();
        myworld.record.reserve(get_num);
        for (unsigned int i = 0; i < get_num; i++)
        {
            myworld.wheresql.clear();
            unsigned int rd_num = rand_range(1, 10000);
            myworld.where("id", rd_num).fetch_append();
        }

        peer->output = myworld.to_json();
        return "";
    }

    //@urlpath(null,fortunes)
    std::string techempowerfortunes(std::shared_ptr<httppeer> peer)
    {
        peer->type("text/html; charset=UTF-8");
        peer->set_header("Date", get_gmttime());

        auto myfortune = orm::Fortune();
        myfortune.fetch();
        myfortune.data.id = 0;
        myfortune.data.message = "Additional fortune added at request time.";
        myfortune.record.push_back(myfortune.data);

        std::sort(myfortune.record.begin(), myfortune.record.end(), [](const auto &lhs, const auto &rhs)
                  { return lhs.message < rhs.message; });
        peer->val["list"].set_array();
        OBJ_ARRAY item;
        for (unsigned int i = 0; i < myfortune.record.size(); i++)
        {
            item["id"] = myfortune.record[i].id;
            item["message"] = html_encode(myfortune.record[i].message);
            peer->val["list"].push(item);
        }

        peer->view("techempower/fortunes");
        return "";
    }

    //@urlpath(null,updates)
    std::string techempowerupdates(std::shared_ptr<httppeer> peer)
    {
        peer->type("application/json; charset=UTF-8");
        peer->set_header("Date", get_gmttime());
        unsigned int get_num = peer->get["queries"].to_int();

        if (get_num == 0)
        {
            get_num = 1;
        }
        else if (get_num > 500)
        {
            get_num = 500;
        }
        auto myworld = orm::World();
        myworld.record.clear();
        myworld.record.reserve(get_num);
        for (unsigned int i = 0; i < get_num; i++)
        {
            myworld.wheresql.clear();
            myworld.where("id", rand_range(1, 10000)).fetch_append();
            if (myworld.effect() > 0)
            {
                unsigned int j = myworld.record.size() - 1;
                myworld.data.randomnumber = rand_range(1, 10000);
                myworld.record[j].randomnumber = myworld.data.randomnumber;
                myworld.update("randomnumber");
            }
        }
        peer->output = myworld.to_json();
        return "";
    }

    //@urlpath(null,cached-queries)
    std::string techempowercached_queries(std::shared_ptr<httppeer> peer)
    {
        peer->type("application/json; charset=UTF-8");
        peer->set_header("Date", get_gmttime());

        unsigned int get_num = peer->get["count"].to_int();
        if (get_num == 0)
        {
            get_num = 1;
        }
        else if (get_num > 500)
        {
            get_num = 500;
        }
        auto myworld = orm::World();
        std::string mycacheid = "alldatacache";

        pzcache<std::vector<orm::worldbase::meta>> &temp_cache = pzcache<std::vector<orm::worldbase::meta>>::conn();

        std::vector<orm::worldbase::meta> allcachedata_array;
        allcachedata_array.reserve(10000);
        // create rand data to cache
        if (temp_cache.check(mycacheid) > -1)
        {
            allcachedata_array = temp_cache.get(mycacheid);
        }
        else
        {
            allcachedata_array.resize(10000);
            for (unsigned int i = 0; i < 10000; i++)
            {
                allcachedata_array[i].id = i + 1;
                allcachedata_array[i].randomnumber = rand_range(1, 10000);
            }
            temp_cache.save(mycacheid, allcachedata_array, 120);
        }
        // get rand data from cache
        mycacheid = "my" + std::to_string(get_num);
        myworld.record.reserve(get_num);
        if (temp_cache.check(mycacheid) > -1)
        {
            myworld.record = temp_cache.get(mycacheid);
        }
        else
        {
            if (allcachedata_array.size() == 10000)
            {
                for (unsigned int i = 0; i < get_num; i++)
                {
                    unsigned int temp_rid = rand_range(0, 9999);
                    myworld.record.push_back(allcachedata_array[temp_rid]);
                }
            }
            temp_cache.save(mycacheid, myworld.record, 120);
        }

        peer->output = myworld.to_json();
        return "";
    }

    //@urlpath(null,cached-db)
    std::string techempowercached_db(std::shared_ptr<httppeer> peer)
    {
        peer->type("application/json; charset=UTF-8");
        peer->set_header("Date", get_gmttime());
        // this test from database to cache
        unsigned int get_num = peer->get["count"].to_int();
        if (get_num == 0)
        {
            get_num = 1;
        }
        else if (get_num > 500)
        {
            get_num = 500;
        }
        auto myworld = orm::World();
        std::string mycacheid = "my" + std::to_string(get_num);

        pzcache<std::vector<orm::worldbase::meta>> &temp_cache = pzcache<std::vector<orm::worldbase::meta>>::conn();

        myworld.record.reserve(get_num);
        if (temp_cache.check(mycacheid) > -1)
        {
            myworld.record = temp_cache.get(mycacheid);
        }
        else
        {
            std::vector<unsigned int> cacheid;
            for (unsigned int i = 0; i < get_num; i++)
            {
                cacheid.push_back(rand_range(1, 10000));
            }

            std::string sqlstr = array_to_sql(cacheid);
            myworld.whereIn("id", sqlstr).fetch();
            temp_cache.save(mycacheid, myworld.record, 120);
        }

        peer->output = myworld.to_json();
        return "";
    }

} // namespace http