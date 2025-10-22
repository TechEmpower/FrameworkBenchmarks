/**
 *
 *  SyncPlugin.cc
 *
 */

#include "SyncPlugin.h"
#include <drogon/drogon.h>

using namespace drogon;

void SyncPlugin::initAndStart(const Json::Value &config)
{
    /// Initialize and start the plugin
    drogon::app().registerSyncAdvice(
        [](const HttpRequestPtr &req) -> HttpResponsePtr {
            if (req->method() != Get)
            {
                return HttpResponsePtr{};
            }
            switch (req->path().length())
            {
                case 5:
                    if (req->path() == "/json")
                    {
                        Json::Value ret;
                        ret["message"] = "Hello, World!";
                        return HttpResponse::newHttpJsonResponse(
                            std::move(ret));
                    }
                    break;
                case 10:
                    if (req->path() == "/plaintext")
                    {
                        auto resp = HttpResponse::newHttpResponse();
                        resp->setBody("Hello, World!");
                        resp->setContentTypeCode(CT_TEXT_PLAIN);
                        return resp;
                    }
                    break;
                default:
                    break;
            }
            return HttpResponsePtr{};
        });
}

void SyncPlugin::shutdown()
{
    /// Shutdown the plugin
}
