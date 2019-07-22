#include "UpdatesCtrlRaw.h"
using namespace drogon::orm;
const void update(
    const std::shared_ptr<std::vector<Result>> &results,
    const std::shared_ptr<std::function<void(const HttpResponsePtr &)>>
        &callbackPtr,
    const DbClientPtr &client)
{
    auto size = results->size();
    std::string sql;
    sql.reserve(256);
    sql.append("update world set randomnumber=case id ");
    int placeholdersCounter = 1;
    for (size_t i = 0; i < size; i++)
    {
        auto tmpStr = drogon::utils::formattedString("when $%d then $%d ",
                                                     placeholdersCounter,
                                                     placeholdersCounter + 1);
        placeholdersCounter += 2;
        sql.append(tmpStr);
    }
    sql.append("else randomnumber end where id in (");
    for (size_t i = 0; i < size; i++)
    {
        auto tmpStr =
            drogon::utils::formattedString("$%d,", placeholdersCounter);
        ++placeholdersCounter;
        sql.append(tmpStr);
    }
    sql[sql.length() - 1] = ')';

    auto sqlBinder = *client << std::move(sql);

    std::string jsonStr;
    jsonStr.reserve(size * 36);
    jsonStr.append("[", 1);
    for (auto const &r : *results)
    {
        auto randId = rand() % 10000 + 1;
        auto id = r[0]["id"].as<int>();
        sqlBinder << id;
        sqlBinder << randId;
        char json[64];
        auto size =
            sprintf(json, "{\"id\":%d,\"randomnumber\":%d}", id, randId);
        jsonStr.append(json, size);
        jsonStr.append(",", 1);
    }
    jsonStr[jsonStr.length() - 1] = ']';
    for (auto const &r : *results)
    {
        sqlBinder << r[0]["id"].as<int>();
    }

    sqlBinder >> [jsonStr = std::move(jsonStr),
                  callbackPtr](const Result &r) mutable {
        auto resp = HttpResponse::newHttpResponse();
        resp->setContentTypeCode(ContentType::CT_APPLICATION_JSON);
        resp->setBody(std::move(jsonStr));
        (*callbackPtr)(resp);
    } >> [callbackPtr](const DrogonDbException &e) {
        Json::Value ret;
        ret["result"] = "error!";
        auto resp = HttpResponse::newHttpJsonResponse(ret);
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
    auto callbackPtr =
        std::make_shared<std::function<void(const HttpResponsePtr &)>>(
            std::move(callback));
    auto resultSetPtr = std::make_shared<std::vector<Result>>();
    resultSetPtr->reserve(queries);
    auto client = app().getFastDbClient();
    for (size_t i = 0; i < queries; i++)
    {
        int id = rand() % 10000 + 1;
        *client << "select * from world where id=$1" << id >>
            [callbackPtr, resultSetPtr, client, queries](
                const Result &r) mutable {
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
