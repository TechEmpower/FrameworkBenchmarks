#pragma once
#include "models/World.h"
#include <drogon/HttpSimpleController.h>
#include <drogon/IOThreadStorage.h>
#include <map>
#include <string>
#include <memory>

using namespace drogon;
using namespace drogon_model::hello_world;

class UpdatesCtrlRaw : public drogon::HttpSimpleController<UpdatesCtrlRaw>
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
    IOThreadStorage<orm::DbClientPtr> dbClient_;
    IOThreadStorage<std::map<size_t, std::unique_ptr<std::string>>> updateSQLs_;
    const std::string &getSQL(size_t count);
    void update(
        const std::shared_ptr<std::vector<World>> &results,
        const std::shared_ptr<std::function<void(const HttpResponsePtr &)>>
            &callbackPtr,
        const DbClientPtr &client);
};
