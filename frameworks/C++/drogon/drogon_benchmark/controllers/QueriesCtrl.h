#pragma once
#include <drogon/HttpSimpleController.h>
#include <drogon/IOThreadStorage.h>

using namespace drogon;
class QueriesCtrl : public drogon::HttpSimpleController<QueriesCtrl>
{
  public:
    virtual void asyncHandleHttpRequest(
        const HttpRequestPtr &req,
        std::function<void(const HttpResponsePtr &)> &&callback) override;
    PATH_LIST_BEGIN
    // list path definitions here;
    // PATH_ADD("/path","filter1","filter2",HttpMethod1,HttpMethod2...);
    PATH_LIST_END
  private:
    IOThreadStorage<orm::DbClientPtr> _dbClient;
};
