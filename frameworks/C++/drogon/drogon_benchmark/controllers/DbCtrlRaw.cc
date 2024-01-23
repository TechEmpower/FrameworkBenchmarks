#include "DbCtrlRaw.h"
#include "World_raw.h"
#include <drogon/utils/Utilities.h>
#include <stdlib.h>
#include <thread>
#include <time.h>

void DbCtrlRaw::asyncHandleHttpRequest(
    const HttpRequestPtr &req,
    std::function<void(const HttpResponsePtr &)> &&callback)
{
    // write your application logic here
    static std::once_flag once;
    std::call_once(once, []() { srand(time(NULL)); });
    if (!*_dbClient)
    {
        *_dbClient = drogon::app().getFastDbClient();
    }
    int id = rand() % 10000 + 1;
    auto callbackPtr =
        std::make_shared<std::function<void(const HttpResponsePtr &)>>(
            std::move(callback));

    **_dbClient << "select * from world where id=$1" << id >>
        [callbackPtr](const Result &rows) {
            if (rows.size() == 1)
            {
                auto obj = World(rows[0]);
                (*callbackPtr)(HttpResponse::newHttpJsonResponse(obj.toJson()));
            }
            else
            {
                Json::Value json{};
                json["code"] = 0;
                json["message"] = "Internal error";
                (*callbackPtr)(
                    HttpResponse::newHttpJsonResponse(std::move(json)));
            }
        } >>
        [callbackPtr](const DrogonDbException &err) {
            Json::Value json{};
            json["code"] = 1;
            json["message"] = err.base().what();
            (*callbackPtr)(HttpResponse::newHttpJsonResponse(std::move(json)));
        };
}
