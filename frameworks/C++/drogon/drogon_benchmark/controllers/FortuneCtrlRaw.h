#pragma once
#include <drogon/HttpSimpleController.h>
#include <drogon/IOThreadStorage.h>

using namespace drogon;
struct Fortune
{
    Fortune(string_view &&id, string_view &&message)
        : id_(std::move(id)), message_(std::move(message))
    {
    }
    Fortune() = default;
    string_view id_;
    string_view message_;
};
class FortuneCtrlRaw : public drogon::HttpSimpleController<FortuneCtrlRaw>
{
  public:
    FortuneCtrlRaw()
        : bodyTemplate_(DrTemplateBase::newTemplate("fortune_raw.csp"))
    {
    }
    virtual void asyncHandleHttpRequest(
        const HttpRequestPtr &req,
        std::function<void(const HttpResponsePtr &)> &&callback) override;
    PATH_LIST_BEGIN
    // list path definitions here;
    PATH_LIST_END
  private:
    IOThreadStorage<orm::DbClientPtr> dbClient_;
    std::shared_ptr<drogon::DrTemplateBase> bodyTemplate_;
};
