#include "UpdatesCtrlRaw.h"
#include <stdlib.h>

using namespace drogon::orm;

const std::string &UpdatesCtrlRaw::getSQL(size_t count)
{
    auto &sqlMap = *updateSQLs_;
    auto iter = sqlMap.find(count);
    if (iter != sqlMap.end())
    {
        return *(iter->second);
    }
    std::string sql;
    sql.reserve(80 + count * 25);
    sql.append("update world set randomnumber=case id ");
    int placeholdersCounter = 1;
    for (size_t i = 0; i < count; i++)
    {
        auto tmpStr = drogon::utils::formattedString("when $%d then $%d ",
                                                     placeholdersCounter,
                                                     placeholdersCounter + 1);
        placeholdersCounter += 2;
        sql.append(tmpStr);
    }
    sql.append("else randomnumber end where id in (");
    for (size_t i = 0; i < count; i++)
    {
        auto tmpStr =
            drogon::utils::formattedString("$%d,", placeholdersCounter);
        ++placeholdersCounter;
        sql.append(tmpStr);
    }
    sql[sql.length() - 1] = ')';
    auto ret =
        sqlMap.insert({count, std::make_unique<std::string>(std::move(sql))});
    return *(ret.first->second);
}
void UpdatesCtrlRaw::update(
    const std::shared_ptr<std::vector<World>> &results,
    const std::shared_ptr<std::function<void(const HttpResponsePtr &)>>
        &callbackPtr,
    const DbClientPtr &client)
{
    auto const &sql = getSQL(results->size());
    auto sqlBinder = *client << string_view(sql.data(), sql.length());
    Json::Value json;
    json.resize(0);
    for (auto const &w : *results)
    {
        auto randId = rand() % 10000 + 1;
        sqlBinder << w.getId();
        sqlBinder << randId;
        Json::Value world;
        world["id"] = w.getId();
        world["randomnumber"] = randId;
        json.append(std::move(world));
    }
    for (auto const &w : *results)
    {
        sqlBinder << w.getId();
    }

    sqlBinder >> [callbackPtr,
                  json = std::move(json)](const Result &r) mutable {
        (*callbackPtr)(HttpResponse::newHttpJsonResponse(std::move(json)));
    } >> [callbackPtr](const DrogonDbException &e) {
        Json::Value json{};
        json["code"] = 1;
        json["message"] = e.base().what();
        auto resp = HttpResponse::newHttpJsonResponse(std::move(json));
        (*callbackPtr)(resp);
    };
}
void UpdatesCtrlRaw::asyncHandleHttpRequest(
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
    auto callbackPtr =
        std::make_shared<std::function<void(const HttpResponsePtr &)>>(
            std::move(callback));
    auto resultSetPtr = std::make_shared<std::vector<World>>();
    resultSetPtr->reserve(queries);
    if (!*dbClient_)
    {
        *dbClient_ = drogon::app().getFastDbClient();
    }
    auto errFlag = std::make_shared<bool>(false);
    for (int i = 0; i < queries; i++)
    {
        int id = rand() % 10000 + 1;
        **dbClient_ << "select * from world where id=$1" << id >>
            [callbackPtr,
             resultSetPtr,
             &client = *dbClient_,
             queries,
             this,
             errFlag](const Result &r) mutable {
                if (*errFlag)
                    return;
                if (r.size() == 1)
                {
                    resultSetPtr->emplace_back(World(r[0]));
                    if (resultSetPtr->size() == queries)
                    {
                        update(resultSetPtr, callbackPtr, client);
                    }
                }
                else
                {
                    if (*errFlag)
                        return;
                    *errFlag = true;
                    Json::Value json{};
                    json["code"] = 0;
                    json["message"] = "Internal error";
                    (*callbackPtr)(
                        HttpResponse::newHttpJsonResponse(std::move(json)));
                }
            } >>
            [callbackPtr, errFlag](const DrogonDbException &e) {
                if (*errFlag)
                    return;
                *errFlag = true;
                Json::Value json{};
                json["code"] = 1;
                json["message"] = e.base().what();
                auto resp = HttpResponse::newHttpJsonResponse(std::move(json));
                (*callbackPtr)(resp);
            };
    }
}
