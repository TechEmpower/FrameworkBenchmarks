#include "UpdatesCtrl.h"
#include "models/World.h"
#include <stdlib.h>

using namespace drogon_model::hello_world;

void UpdatesCtrl::asyncHandleHttpRequest(
    const HttpRequestPtr &req,
    std::function<void(const HttpResponsePtr &)> &&callback)
{
    // write your application logic here
    static std::once_flag once;
    std::call_once(once, []() { srand(time(NULL)); });
    int queries = 1;
    auto &parameter = req->getParameter("queries");
    if (!parameter.empty())
    {
        queries = atoi(parameter.c_str());
        if (queries > 500)
            queries = 500;
        else if (queries < 1)
            queries = 1;
    }
    auto json = std::make_shared<Json::Value>();
    json->resize(0);
    auto callbackPtr =
        std::make_shared<std::function<void(const HttpResponsePtr &)>>(
            std::move(callback));
    auto counter = std::make_shared<int>(queries);
    if (!*_dbClient)
    {
        *_dbClient = drogon::app().getFastDbClient();
    }
    drogon::orm::Mapper<World> mapper(*_dbClient);

    for (int i = 0; i < queries; i++)
    {
        World::PrimaryKeyType id = rand() % 10000 + 1;
        mapper.findByPrimaryKey(
            id,
            [callbackPtr, counter, json, &client = *_dbClient](
                World w) mutable {
                w.setRandomnumber(rand() % 10000 + 1);
                drogon::orm::Mapper<World> mapper(client);
                mapper.update(
                    w,
                    [w,
                     json = std::move(json),
                     counter = std::move(counter),
                     callbackPtr](const size_t count) {
                        json->append(w.toJson());
                        (*counter)--;
                        if ((*counter) == 0)
                        {
                            (*callbackPtr)(
                                HttpResponse::newHttpJsonResponse(*json));
                        }
                    },
                    [callbackPtr](const DrogonDbException &e) {
                        Json::Value ret;
                        ret["result"] = "error!";
                        auto resp = HttpResponse::newHttpJsonResponse(ret);
                        (*callbackPtr)(resp);
                    });
            },
            [callbackPtr](const DrogonDbException &e) {
                Json::Value ret;
                ret["result"] = "error!";
                auto resp = HttpResponse::newHttpJsonResponse(ret);
                (*callbackPtr)(resp);
            });
    }
}