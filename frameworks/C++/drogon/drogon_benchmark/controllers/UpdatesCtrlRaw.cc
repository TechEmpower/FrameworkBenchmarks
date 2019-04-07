#include "UpdatesCtrlRaw.h"
using namespace drogon::orm;
const void update(const std::shared_ptr<std::vector<Result>> &results,
                  const std::shared_ptr<std::function<void(const HttpResponsePtr &)>> &callbackPtr,
                  const DbClientPtr &client)
{

    auto size = results->size();
    std::string sql;
    sql.reserve(256);
    sql.append("update world set randomnumber=case id ");
    int placeholdersCounter = 1;
    for (size_t i = 0; i < size; i++)
    {
        auto tmpStr = drogon::utils::formattedString("when $%d then $%d ", placeholdersCounter, placeholdersCounter + 1);
        placeholdersCounter += 2;
        sql.append(tmpStr);
    }
    sql.append("else randomnumber end where id in (");
    for (size_t i = 0; i < size; i++)
    {
        auto tmpStr = drogon::utils::formattedString("$%d,", placeholdersCounter);
        ++placeholdersCounter;
        sql.append(tmpStr);
    }
    sql[sql.length() - 1] = ')';

    auto sqlBinder = *client << std::move(sql);

    auto jsonPtr = std::make_shared<Json::Value>();
    jsonPtr->resize(0);
    for (auto &r : *results)
    {
        auto randId = rand() % 10000 + 1;
        auto id = r[0]["id"].as<int>();
        sqlBinder << id;
        sqlBinder << randId;
        Json::Value j;
        j["id"] = id;
        j["randomnumber"] = randId;
        jsonPtr->append(std::move(j));
    }
    for (auto &r : *results)
    {
        sqlBinder << r[0]["id"].as<int>();
    }

    sqlBinder >>
        [jsonPtr = std::move(jsonPtr), callbackPtr](const Result &r) mutable {
            (*callbackPtr)(HttpResponse::newHttpJsonResponse(*jsonPtr));
        } >>
        [callbackPtr](const DrogonDbException &e) {
            Json::Value ret;
            ret["result"] = "error!";
            auto resp = HttpResponse::newHttpJsonResponse(ret);
            (*callbackPtr)(resp);
        };
}
void UpdatesCtrlRaw::asyncHandleHttpRequest(const HttpRequestPtr &req, const std::function<void(const HttpResponsePtr &)> &callback)
{
    //write your application logic here
    static std::once_flag once;
    std::call_once(once, []() {
        srand(time(NULL));
    });
    size_t queries = 1;
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
    auto callbackPtr = std::shared_ptr<std::function<void(const HttpResponsePtr &)>>(new std::function<void(const HttpResponsePtr &)>(callback));
    auto resultSetPtr = std::make_shared<std::vector<Result>>();
    resultSetPtr->reserve(queries);
    auto client = app().getFastDbClient();
    for (size_t i = 0; i < queries; i++)
    {
        int id = rand() % 10000 + 1;
        *client << "select * from world where id=$1"
                << id >>
            [callbackPtr, resultSetPtr, client, queries](const Result &r) mutable {
                resultSetPtr->push_back(r);
                if (resultSetPtr->size() == queries)
                {
                    update(resultSetPtr, callbackPtr, client);
                }
            } >>
            [callbackPtr](const DrogonDbException &e) {
                Json::Value ret;
                ret["result"] = "error!";
                auto resp = HttpResponse::newHttpJsonResponse(ret);
                (*callbackPtr)(resp);
            };
    }
}
